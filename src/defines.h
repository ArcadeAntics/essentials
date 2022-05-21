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


#define R_print(X) (eval(lang2(install("print"), lang2(install("quote"), (X))), R_BaseEnv))


#define do_return(X)                                                   \
    {                                                                  \
        return_this = (X);                                             \
        UNPROTECT(np);                                                 \
        return return_this;                                            \
    }
