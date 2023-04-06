#include <R.h>
#include <Rinternals.h>


// #define debug


#include "getFromBase.h"


#include "defines.h"  // includes R_print() and set_R_Visible()





/*
 * asChar() on a factor does asChar() on the integer codes, not the
 * corresponding level. this fixes that for our specific need
 */
#define factor_to_CHARSXP(X) do {                              \
    int code = asInteger((X));                                 \
    if (code == NA_INTEGER)                                    \
        (X) = NA_STRING;                                       \
    else {                                                     \
        SEXP levels = getAttrib((X), R_LevelsSymbol);          \
        if (TYPEOF(levels) != STRSXP)                          \
            error("malformed factor");                         \
        if (code >= 1 && code <= LENGTH(levels))               \
            (X) = STRING_ELT(levels, code - 1);                \
        else error("malformed factor");                        \
    }                                                          \
} while (0)





SEXP do_intequal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP e1 = CADR(args),
         e2 = CADDR(args);
    if (!isInteger(e1) || LENGTH(e1) != 1) {
        error("invalid 'e1'");
        return ScalarLogical(FALSE);
    }
    if (!isVector(e2) || LENGTH(e2) != 1) {
        error("'e2' must be a length 1 vector");
        return ScalarLogical(FALSE);
    }
    return ScalarLogical(INTEGER(e1)[0] == asInteger(e2));
}





SEXP do_strequal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP equalsSymbol = NULL;
    if (equalsSymbol == NULL) {
        equalsSymbol = install("==");
    }


    SEXP e1 = CADR(args),
         e2 = CADDR(args);
    if (!isString(e1) || LENGTH(e1) != 1) {
        error("invalid 'e1'");
        return ScalarLogical(FALSE);
    }
    e1 = STRING_ELT(e1, 0);
    if (!isVector(e2) || LENGTH(e2) != 1) {
        error("'e2' must be a length 1 vector");
        return ScalarLogical(FALSE);
    }
    if (isFactor(e2)) {
        factor_to_CHARSXP(e2);
    }
    else {
        e2 = asChar(e2);
    }
    if (e1 == NA_STRING)
        return ScalarLogical(e2 == NA_STRING);
    else if (e2 == NA_STRING)
        return ScalarLogical(FALSE);
    else {
        SEXP expr = PROTECT(lang3(
            findVarInFrame(R_BaseEnv, equalsSymbol),
            ScalarString(e1),  // don't want to call method dispatch
            ScalarString(e2)
        ));
        SEXP value = eval(expr, R_BaseEnv);
        UNPROTECT(1);
        return value;
    }
}





SEXP do_strcaseequal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP equalsSymbol = NULL,
                tolowerSymbol = NULL;
    if (equalsSymbol == NULL) {
        equalsSymbol = install("==");
        tolowerSymbol = install("tolower");
    }


    SEXP e1 = CADR(args),
         e2 = CADDR(args);
    if (!isString(e1) || LENGTH(e1) != 1) {
        error("invalid 'e1'");
        return ScalarLogical(FALSE);
    }
    e1 = STRING_ELT(e1, 0);
    if (!isVector(e2) || LENGTH(e2) != 1) {
        error("'e2' must be a length 1 vector");
        return ScalarLogical(FALSE);
    }
    if (isFactor(e2)) {
        factor_to_CHARSXP(e2);
    }
    else {
        e2 = asChar(e2);
    }
    if (e1 == NA_STRING)
        return ScalarLogical(e2 == NA_STRING);
    else if (e2 == NA_STRING)
        return ScalarLogical(FALSE);
    else {
        SEXP expr = PROTECT(lang3(
            equalsSymbol,
            lang2(tolowerSymbol, ScalarString(e1)), // don't want to call method dispatch
            lang2(tolowerSymbol, ScalarString(e2))
        ));
        SEXP value = eval(expr, R_BaseEnv);
        UNPROTECT(1);
        return value;
    }
}





#ifdef debug
    #define define_vars do {                                       \
        cont = contSymbol;                                         \
        first_time = first_timeSymbol;                             \
    } while (0)
    #define R_print_and_error do {                                 \
        Rprintf("\n> expr\n");                                     \
        R_print(expr);                                             \
        error("stopped for debugging purposes");                   \
        return R_NilValue;                                         \
    } while (0)
#else
    #define define_vars do {                                       \
        cont       = PROTECT(allocVector(LGLSXP, 1)); nprotect++;  \
        first_time = PROTECT(allocVector(LGLSXP, 1)); nprotect++;  \
        LOGICAL(cont)[0] = FALSE;                                  \
        LOGICAL(first_time)[0] = TRUE;                             \
    } while (0)
    #define R_print_and_error do {} while (0)
#endif





#define jswitch_beginning(equal)                               \
    SEXP cont, first_time;                                     \
    define_vars;                                               \
    SEXP expr = R_NilValue;                                    \
    PROTECT_INDEX expr_index;                                  \
    PROTECT_WITH_INDEX(expr, &expr_index); nprotect++;         \
    REPROTECT(expr = LCONS(lang1(getFromBase(breakSymbol)), expr), expr_index);\
    SEXP dot, ccase, eexpr;                                    \
    int nprotect2,                                             \
        dots_indx = dots_length - 1,                           \
        last_dot = 1;                                          \
    for (; dots_indx >= 0; dots_indx--) {                      \
        nprotect2 = 0;                                         \
        dot = nthcdr(dots, dots_indx);                         \
        if (!isNull(TAG(CDR(dot))))                            \
            error("invalid cases, do not provide argument names");\
        dot = PREXPR(CAR(dot));                                \
        if (last_dot) {                                        \
            last_dot = 0;                                      \
            if (TYPEOF(dot) == SYMSXP) {                       \
                if (dot == defaultSymbol)                      \
                    continue;                                  \
                else error("invalid case call");               \
            }                                                  \
            if (TYPEOF(dot) == LANGSXP &&                      \
                length(dot) == 3L &&                           \
                CAR(dot) == walrusSymbol &&                    \
                CADR(dot) == defaultSymbol)                    \
            {                                                  \
                REPROTECT(expr = LCONS(CADDR(dot), expr), expr_index);\
                continue;                                      \
            }                                                  \
        }                                                      \
        if (!(TYPEOF(dot) == LANGSXP &&                        \
              TYPEOF(CAR(dot)) == SYMSXP))                     \
        {                                                      \
            error("invalid case call");                        \
            return R_NilValue;                                 \
        }                                                      \
        eexpr = PROTECT(lang3(                                 \
            assign_in_place,                                   \
            cont,                                              \
            ScalarLogical(TRUE)                                \
        )); nprotect2++;                                       \
        if (CAR(dot) == walrusSymbol) {                        \
            if (length(dot) != 3) {                            \
                error("invalid case call");                    \
                return R_NilValue;                             \
            }                                                  \
            ccase = CADR(dot);                                 \
            eexpr = PROTECT(lang3(                             \
                getFromBase(R_BraceSymbol),                    \
                eexpr,                                         \
                CADDR(dot)                                     \
            )); nprotect2++;                                   \
        }                                                      \
        else {                                                 \
            ccase = dot;                                       \
        }                                                      \
        if (!(TYPEOF(ccase) == LANGSXP &&                      \
              TYPEOF(CAR(ccase)) == SYMSXP))                   \
        {                                                      \
            error("invalid case call");                        \
            return R_NilValue;                                 \
        }                                                      \
        if (CAR(ccase) == caseSymbol &&                        \
            length(ccase) == 2)                                \
        {                                                      \
            ccase = PROTECT(lang3(equal, EXPR, CADR(ccase)));  \
            nprotect2++;                                       \
        }


#define jswitch_end                                            \
        else {                                                 \
            error("invalid case call");                        \
            return R_NilValue;                                 \
        }                                                      \
        REPROTECT(expr = LCONS(lang3(                          \
            getFromBase(ifSymbol),                             \
            lang3(                                             \
                getFromBase(orSymbol),                         \
                cont,                                          \
                ccase                                          \
            ),                                                 \
            eexpr                                              \
        ), expr), expr_index);                                 \
        UNPROTECT(nprotect2);                                  \
    }                                                          \
    REPROTECT(expr = LCONS(lang3(                              \
        assign_in_place,                                       \
        first_time,                                            \
        ScalarLogical(FALSE)                                   \
    ), expr), expr_index);                                     \
    REPROTECT(expr = LCONS(lang4(                              \
        getFromBase(ifSymbol),                                 \
        first_time,                                            \
        lang1(getFromBase(R_BraceSymbol)),                     \
        lang2(                                                 \
            getFromBase(stopSymbol),                           \
            mkString("cannot use next within 'jswitch'")       \
        )                                                      \
    ), expr), expr_index);                                     \
    REPROTECT(expr = LCONS(getFromBase(R_BraceSymbol), expr), expr_index);\
    REPROTECT(expr = lang2(getFromBase(repeatSymbol), expr), expr_index);\
    R_print_and_error;                                         \
    eval(expr, parent_frame);





#define jswitch_str do {                                       \
    EXPR = PROTECT(ScalarString(EXPR)); nprotect++;            \
    jswitch_beginning(strequal)                                \
        else if (CAR(ccase) == icaseSymbol &&                  \
                 length(ccase) == 2)                           \
        {                                                      \
            ccase = PROTECT(lang3(strcaseequal, EXPR, CADR(ccase)));\
            nprotect2++;                                       \
        }                                                      \
        else if (CAR(ccase) == recaseSymbol &&                 \
                 length(ccase) >= 2)                           \
        {                                                      \
            ccase = PROTECT(LCONS(EXPR, CDR(ccase))); nprotect2++;\
            SET_TAG(ccase, xSymbol);                           \
            ccase = PROTECT(LCONS(getFromBase(greplSymbol), ccase)); nprotect2++;\
            ccase = PROTECT(lang2(getFromBase(isTRUESymbol), ccase)); nprotect2++;\
        }                                                      \
    jswitch_end                                                \
} while (0)


#define jswitch_int do {                                       \
    EXPR = PROTECT(ScalarInteger(asInteger(EXPR))); nprotect++;\
    jswitch_beginning(intequal)                                \
    jswitch_end                                                \
} while (0)





SEXP do_jswitch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP contSymbol = NULL,
#ifdef debug
                first_timeSymbol = NULL,
                setSymbol = NULL,
#endif
                breakSymbol = NULL,
                defaultSymbol = NULL,
                walrusSymbol = NULL,
                caseSymbol = NULL,
                ifSymbol = NULL,
                orSymbol = NULL,
                stopSymbol = NULL,
                repeatSymbol = NULL,
                icaseSymbol = NULL,
                recaseSymbol = NULL,
                xSymbol = NULL,
                greplSymbol = NULL,
                isTRUESymbol = NULL,
                parent_frameSymbol = NULL,
                intequalSymbol = NULL,
                strequalSymbol = NULL,
                strcaseequalSymbol = NULL,
                assign_in_placeSymbol = NULL;
    if (contSymbol == NULL) {
        contSymbol = install("cont");
#ifdef debug
        first_timeSymbol = install("first_time");
        setSymbol = install("<-");
#endif
        breakSymbol = install("break");
        defaultSymbol = install("default");
        walrusSymbol = install(":=");
        caseSymbol = install("case");
        ifSymbol = install("if");
        orSymbol = install("||");
        stopSymbol = install("stop");
        repeatSymbol = install("repeat");
        icaseSymbol = install("icase");
        recaseSymbol = install("recase");
        xSymbol = install("x");
        greplSymbol = install("grepl");
        isTRUESymbol = install("isTRUE");
        parent_frameSymbol = install("parent.frame");
        intequalSymbol = install("intequal");
        strequalSymbol = install("strequal");
        strcaseequalSymbol = install("strcaseequal");
        assign_in_placeSymbol = install("assign.in.place");
    }


    int nprotect = 0;


    SEXP parent_frame;
    parent_frame = PROTECT(lang1(parent_frameSymbol)); nprotect++;
    parent_frame = PROTECT(eval(parent_frame, rho)); nprotect++;


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue) {
        error("something is wrong with 'jswitch'");
        UNPROTECT(nprotect);
        return R_NilValue;
    }


    int dots_length = ((TYPEOF(dots) == DOTSXP) ? length(dots) : 0);


    if (dots_length <= 0) {
        error("'EXPR' is missing");
        UNPROTECT(nprotect);
        return R_NilValue;
    }


    SEXP EXPR = PROTECT(eval(CAR(dots), rho)); nprotect++;
    dots = CDR(dots); dots_length--;


    if (!isVector(EXPR) || LENGTH(EXPR) != 1) {
        error("'EXPR' must be a length 1 vector");
        UNPROTECT(nprotect);
        return R_NilValue;
    }


    if (dots_length <= 0) {
        UNPROTECT(nprotect);
        set_R_Visible(0);
        return R_NilValue;
    }


    SEXP assign_in_place, intequal, strequal, strcaseequal;
#ifdef debug
    assign_in_place = setSymbol;
    intequal = intequalSymbol;
    strequal = strequalSymbol;
    strcaseequal = strcaseequalSymbol;
#else
    assign_in_place = PROTECT(eval(assign_in_placeSymbol, rho)); nprotect++;
    intequal        = PROTECT(eval(intequalSymbol       , rho)); nprotect++;
    strequal        = PROTECT(eval(strequalSymbol       , rho)); nprotect++;
    strcaseequal    = PROTECT(eval(strcaseequalSymbol   , rho)); nprotect++;
#endif


    if (isFactor(EXPR)) {
        factor_to_CHARSXP(EXPR);
        jswitch_str;
    }
    else if (isString(EXPR)) {
        EXPR = STRING_ELT(EXPR, 0);
        jswitch_str;
    }
    else {
        jswitch_int;
    }


    set_R_Visible(0);
    return R_NilValue;
}
