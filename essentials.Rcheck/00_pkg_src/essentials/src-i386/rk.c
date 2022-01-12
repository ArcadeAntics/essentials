#include <R.h>
#include <Rinternals.h>
#include "defines.h"


SEXP RK4(SEXP independent, SEXP initialConditions, SEXP fun, SEXP rho)
{
    /*
    test4NumericArgument(independent);
    test4NumericArgument(initialConditions);
    if (!isFunction(fun))    error("invalid '%s'", "fun");
    if (!isEnvironment(rho)) error("invalid '%s'", "rho");

    SEXP R_fcall = PROTECT(lang3(
        install("+"),
        install("x"),
        lang3(
            install("*"),
            lang3(
                install("/"),
                install("h1"),
                ScalarReal(6)),
            lang3(
                install("+"),
                lang3(
                  install("+"),
                  lang3(
                    install("+"),
                    install("k1"),
                    lang3(
                      install("*"),
                      ScalarReal(2),
                      install("k2"))),
                  lang3(
                    install("*"),
                    ScalarReal(2),
                    install("k3"))),
                lang3(
                  install("fun"),
                  lang3(
                    install("+"),
                    install("t"),
                    install("h1")),
                  lang3(
                    install("+"),
                    install("x"),
                    lang3(
                      install("*"),
                      install("h1"),
                      lang3(
                        install("<-"),
                        install("k3"),
                        lang3(
                          install("fun"),
                          lang3(
                            install("+"),
                            install("t"),
                            install("h2")),
                          lang3(
                            install("+"),
                            install("x"),
                            lang3(
                              install("*"),
                              install("h2"),
                              lang3(
                                install("<-"),
                                install("k2"),
                                lang3(
                                  install("fun"),
                                  lang3(
                                    install("+"),
                                    install("t"),
                                    install("h2")),
                                  lang3(
                                    install("+"),
                                    install("x"),
                                    lang3(
                                      install("*"),
                                      install("h2"),
                                      lang3(
                                        install("<-"),
                                        install("k1"),
                                        lang3(
                                          install("fun"),
                                          install("t"),
                                          install("x"))))))))))))))))));

    SEXP value = PROTECT(allocVector(VECSXP, xlength(initialConditions) + 1));
    SET_VECTOR_ELT(value, 0, Rf_lazy_duplicate(independent));

    SEXP h = PROTECT(eval(lang2(install("diff"), independent), rho));
    int n, len_h = xlength(h);

    double t, h1, h2, *rh = REAL(h), *rindependent = REAL(independent);

    for (n = 0; n < len_h; n++) {
        t = rindependent[n];
        h1 = rh[n];
        h2 = h1/2;
        eval(R_fcall, rho);
    }

    UNPROTECT(2);
    return rho;
    return h;
     */
    return R_NilValue;
}
