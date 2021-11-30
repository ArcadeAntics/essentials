#define test4NumericArgument(X)                                \
    switch(TYPEOF(X)) {                                        \
    case INTSXP:                                               \
    case LGLSXP:                                               \
    case NILSXP:                                               \
    case REALSXP:                                              \
        break;                                                 \
    default:                                                   \
        error("Non-numeric argument to mathematical function");\
    }


#define streql(str1, str2) (strcmp(str1, str2) == 0)


#define cISNAN(X) (ISNAN(X.r) || ISNAN(X.i))
