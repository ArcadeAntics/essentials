#ifdef debug
    #define getFromBase(X) ((X))
#else
    #define getFromBase(X) (findVarInFrame(R_BaseEnv, (X)))
#endif
