#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "essentials.h"


static const R_ExternalMethodDef externalRoutines[] = {


    /* assigninplace.c */


    {"assigninplace", (DL_FUNC) &do_assigninplace, 2},


    /* doexpr.c */


    {"doexpr", (DL_FUNC) &do_doexpr, 2},


    /* dowhile.c */


    {"dowhile", (DL_FUNC) &do_dowhile, 0},
    {"dountil", (DL_FUNC) &do_dountil, 0},


    /* envvars.c */


    {"envvars"  , (DL_FUNC) &do_envvars  , 0},
    {"getenvvar", (DL_FUNC) &do_getenvvar, 1},


    /* fstr.c */


    {"mkpromise"     , (DL_FUNC) &do_mkpromise     , 2},
    {"delayedassign2", (DL_FUNC) &do_delayedassign2, 4},
    {"asenv"         , (DL_FUNC) &do_asenv         , 2},
    {"fstrold"       , (DL_FUNC) &do_fstrold       , 4},
    {"fstr"          , (DL_FUNC) &do_fstr          , 4},


    /* gev.c */


    {"dgev", (DL_FUNC) &do_dgev, 5},
    {"pgev", (DL_FUNC) &do_pgev, 6},
    {"qgev", (DL_FUNC) &do_qgev, 6},
    {"rgev", (DL_FUNC) &do_rgev, 4},


    /* gpd.c */


    {"dgpd", (DL_FUNC) &do_dgpd, 5},
    {"pgpd", (DL_FUNC) &do_pgpd, 6},
    {"qgpd", (DL_FUNC) &do_qgpd, 6},
    {"rgpd", (DL_FUNC) &do_rgpd, 4},


    /* hypot.c */


    {"phypot", (DL_FUNC) &do_phypot, 1},
    {"hypot" , (DL_FUNC) &do_hypot , 1},

    {"idw", (DL_FUNC) &do_idw, 4},


    /* ismissingarg.c */


    {"ismissingarg", (DL_FUNC) &do_ismissingarg, 0},


    /* jswitch.c */


    {"intequal"    , (DL_FUNC) &do_intequal    , 2},
    {"strequal"    , (DL_FUNC) &do_strequal    , 2},
    {"strcaseequal", (DL_FUNC) &do_strcaseequal, 2},
    {"jswitch"     , (DL_FUNC) &do_jswitch     , 0},


    /* mfor.c */


    {"mfor"      , (DL_FUNC) &do_mfor      , 0},
    {"ismfordone", (DL_FUNC) &do_ismfordone, 1},


    /* plapply.c */


    {"plapply", (DL_FUNC) &do_plapply, -1},
    {"pvapply", (DL_FUNC) &do_pvapply, -1},


    /* rk4.c */


    {"rk4", (DL_FUNC) &do_rk4, 4},


    /* rowmatch.c */


    {"matchtype"        , (DL_FUNC) &do_matchtype        , 2},
    {"rowmatchdataframe", (DL_FUNC) &do_rowmatchdataframe, 4},


    /* scalars.c */


    {"asscalarlogical"  , (DL_FUNC) &do_asscalarlogical  , 1},
    {"asscalarinteger"  , (DL_FUNC) &do_asscalarinteger  , 1},
    {"asscalardouble"   , (DL_FUNC) &do_asscalardouble   , 1},
    {"asscalarcomplex"  , (DL_FUNC) &do_asscalarcomplex  , 1},
    {"asscalarcharacter", (DL_FUNC) &do_asscalarcharacter, 1},
    {"asscalarraw"      , (DL_FUNC) &do_asscalarraw      , 1},
    {"asscalarnumber"   , (DL_FUNC) &do_asscalarnumber   , 2},

    {"asscalar"         , (DL_FUNC) &do_asscalar         , 2},
    {"isscalar"         , (DL_FUNC) &do_isscalar         , 2},
    {"asnumbers"        , (DL_FUNC) &do_asnumbers        , 2},


    /* seq2.c */


    {"seq2", (DL_FUNC) &do_seq2,  0},


    /* unpackset.c */


    {"unpackset", (DL_FUNC) &do_unpackset, 3},


    {NULL, NULL, 0}
};


void attribute_visible R_init_essentials(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
