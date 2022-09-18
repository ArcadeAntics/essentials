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

extern SEXP do_as_numbers(SEXP x, SEXP strict);

extern SEXP do_hypot(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_phypot(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP testing(SEXP args);

extern SEXP do_IDW(SEXP x0, SEXP u0, SEXP x, SEXP p);
extern SEXP do_RK4(SEXP independent, SEXP initialConditions, SEXP fun, SEXP rho);

extern SEXP do_rowmatchdataframe(SEXP x, SEXP table, SEXP nomatch, SEXP incomparables, SEXP rho);

extern SEXP testing(SEXP x);

extern SEXP do_match_type(SEXP x, SEXP table);

extern SEXP MissingArg();
extern SEXP do_isMissingArg(SEXP x, SEXP rho);

extern SEXP do_tryExcept_onexit_setup(SEXP x, SEXP rho);

extern SEXP do_dowhile(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_dountil(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_envvars(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_getEnvvar(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_dgev(SEXP x, SEXP mu, SEXP sigma, SEXP xi, SEXP give_log);
extern SEXP do_pgev(SEXP q, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP do_qgev(SEXP p, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP do_rgev(SEXP n, SEXP mu, SEXP sigma, SEXP xi);

extern SEXP do_dgpd(SEXP x, SEXP mu, SEXP sigma, SEXP xi, SEXP give_log);
extern SEXP do_pgpd(SEXP q, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP do_qgpd(SEXP p, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p);
extern SEXP do_rgpd(SEXP n, SEXP mu, SEXP sigma, SEXP xi);

extern SEXP do_plapply(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pvapply(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_ismfordone(SEXP rho);
extern SEXP do_mfor(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_mkPROMISE(SEXP value, SEXP env);
extern SEXP do_as_env(SEXP envir, SEXP enclos, SEXP context);
extern SEXP do_f_str_old(SEXP sprintf, SEXP fmt, SEXP exprs, SEXP envir);
extern SEXP do_f_str(SEXP x, SEXP rho, SEXP simplify);

extern SEXP do_doexpr(SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_unpackset(SEXP call, SEXP op, SEXP x, SEXP value, SEXP rho);

extern SEXP do_assigninplace(SEXP x, SEXP value);

extern SEXP do_PREXPR(SEXP x, SEXP rho);
extern SEXP do_PRENV(SEXP x, SEXP rho);
extern SEXP do_PRINFO(SEXP x, SEXP rho);

extern SEXP do_intequal(SEXP e1, SEXP e2);
extern SEXP do_strequal(SEXP e1, SEXP e2);
extern SEXP do_strcaseequal(SEXP e1, SEXP e2);

extern SEXP do_jswitch(SEXP call, SEXP op, SEXP args, SEXP rho);
