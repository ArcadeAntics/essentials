#include <Rinternals.h>


/* assigninplace.c */


extern SEXP do_assigninplace(SEXP call, SEXP op, SEXP args, SEXP rho);


/* doexpr.c */


extern SEXP do_doexpr(SEXP call, SEXP op, SEXP args, SEXP rho);


/* dowhile.c */


extern SEXP do_dowhile(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_dountil(SEXP call, SEXP op, SEXP args, SEXP rho);


/* envvars.c */


extern SEXP do_envvars  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_getenvvar(SEXP call, SEXP op, SEXP args, SEXP rho);


/* fstr.c */


extern SEXP do_mkpromise     (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_delayedassign2(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asenv         (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_fstrold       (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_fstr          (SEXP call, SEXP op, SEXP args, SEXP rho);


/* gev.c */


extern SEXP do_dgev(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pgev(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_qgev(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_rgev(SEXP call, SEXP op, SEXP args, SEXP rho);


/* gpd.c */


extern SEXP do_dgpd(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pgpd(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_qgpd(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_rgpd(SEXP call, SEXP op, SEXP args, SEXP rho);


/* hypot.c */


extern SEXP do_phypot(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_hypot (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_idw   (SEXP call, SEXP op, SEXP args, SEXP rho);


/* ismissingarg.c */


extern SEXP do_ismissingarg(SEXP call, SEXP op, SEXP args, SEXP rho);


/* jswitch.c */


extern SEXP do_intequal    (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_strequal    (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_strcaseequal(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_jswitch     (SEXP call, SEXP op, SEXP args, SEXP rho);


/* mfor.c */


extern SEXP do_mfor      (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_ismfordone(SEXP call, SEXP op, SEXP args, SEXP rho);


/* plapply.c */


extern SEXP do_plapply(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_pvapply(SEXP call, SEXP op, SEXP args, SEXP rho);


/* rk4.c */


extern SEXP do_rk4(SEXP call, SEXP op, SEXP args, SEXP rho);


/* rowmatch.c */


extern SEXP do_matchtype        (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_rowmatchdataframe(SEXP call, SEXP op, SEXP args, SEXP rho);


/* scalars.c */


extern SEXP do_asscalarlogical  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalarinteger  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalardouble   (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalarcomplex  (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalarcharacter(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalarraw      (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asscalarnumber   (SEXP call, SEXP op, SEXP args, SEXP rho);

extern SEXP do_asscalar         (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_isscalar         (SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP do_asnumbers        (SEXP call, SEXP op, SEXP args, SEXP rho);


/* seq2.c */


extern SEXP do_seq2(SEXP call, SEXP op, SEXP args, SEXP rho);


/* unpackset.c */


extern SEXP do_unpackset(SEXP call, SEXP op, SEXP args, SEXP rho);
