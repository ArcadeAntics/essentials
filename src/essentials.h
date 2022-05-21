#include <Rinternals.h>


extern SEXP do_asscalarlogical(SEXP x);
extern SEXP do_asscalarinteger(SEXP x);
extern SEXP do_asscalarreal   (SEXP x);
extern SEXP do_asscalarcomplex(SEXP x);
extern SEXP do_asscalarstring (SEXP x);
extern SEXP do_asscalarraw    (SEXP x);

extern SEXP do_asscalarnumber (SEXP x, SEXP strict);

extern SEXP do_asscalar(SEXP x, SEXP mode);

extern SEXP do_isscalar(SEXP x, SEXP mode);

extern SEXP as_numbers(SEXP x, SEXP strict);

extern SEXP do_hypot(SEXP args);
extern SEXP do_phypot(SEXP args);
extern SEXP testing(SEXP args);

extern SEXP IDW(SEXP x0, SEXP u0, SEXP x, SEXP p);
extern SEXP RK4(SEXP independent, SEXP initialConditions, SEXP fun, SEXP rho);

extern SEXP do_rowmatchdataframe(SEXP x, SEXP table, SEXP nomatch, SEXP incomparables, SEXP rho);

extern SEXP testing(SEXP x);

extern SEXP do_match_type(SEXP x, SEXP table);

extern SEXP MissingArg();
extern SEXP do_isMissingArg(SEXP x, SEXP rho);

extern SEXP do_tryExcept_onexit_setup(SEXP x, SEXP rho);

extern SEXP do_dowhile(SEXP expr, SEXP cond, SEXP until, SEXP rho);

extern SEXP do_envvars(SEXP args, SEXP visible);
extern SEXP do_getEnvvar(SEXP x, SEXP default_);

extern SEXP dgev(SEXP x, SEXP mu, SEXP sigma, SEXP xi, SEXP give_log);
extern SEXP pgev(SEXP q, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP qgev(SEXP p, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP rgev(SEXP n, SEXP mu, SEXP sigma, SEXP xi);

extern SEXP do_plapply(SEXP X, SEXP FUN, SEXP dots_as_DOTSXP, SEXP rho);
extern SEXP do_pvapply(SEXP X, SEXP FUN, SEXP FUN_VALUE, SEXP USE_NAMES,
    SEXP dots_as_DOTSXP, SEXP rho);

extern SEXP do_seq(SEXP rho);

extern SEXP do_is_mfor_done(SEXP rho);
extern SEXP do_mfor(SEXP rho, SEXP p);

extern SEXP do_mkPROMISE(SEXP value, SEXP env);
extern SEXP do_as_env(SEXP envir, SEXP enclos, SEXP context);
extern SEXP do_f_str(SEXP sprintf, SEXP fmt, SEXP exprs, SEXP envir);

SEXP do_do_expr(SEXP sexpr, SEXP rho);
