#define test4NumericArgument(X)                                \
    switch (TYPEOF((X))) {                                     \
    case INTSXP:                                               \
    case LGLSXP:                                               \
    case NILSXP:                                               \
    case REALSXP:                                              \
        break;                                                 \
    default:                                                   \
        error("Non-numeric argument to mathematical function");\
    }


#define streql(str1, str2) (strcmp((str1), (str2)) == 0)


#define cISNAN(X) (ISNAN((X).r) || ISNAN((X).i))


#define UNIMPLEMENTED_TYPE(X, Y) (error("unimplemented type '%s' in '%s'", type2char(TYPEOF((Y))), (X)))


#define enquote(X) (lang2(findVarInFrame(R_BaseEnv, R_QuoteSymbol), (X)))


#define R_print(X) (eval(lang2(findVarInFrame(R_BaseEnv, install("print")), enquote((X))), R_BaseEnv))


#define _is_formula(X) (                                               \
    inherits((X), "formula") &&                                        \
        (CAR((X)) == install("~") ||                                   \
         CAR((X)) == findVarInFrame(R_BaseEnv, install("~")))          \
)


#define quoteLang(X) (                                                 \
    (X) == R_MissingArg ? R_MissingArg : (                             \
        TYPEOF((X)) == SYMSXP || (TYPEOF((X)) == LANGSXP && !_is_formula((X))) ? lang2(findVarInFrame(R_BaseEnv, install("quote")), (X)) : (X)\
    )                                                                  \
)


#define dotsLength(dots) ((TYPEOF((dots)) == DOTSXP) ? length((dots)) : 0)


#define set_R_Visible(X) (eval( (X) ? R_NilValue : lang1(install("invisible")) , R_BaseEnv))
