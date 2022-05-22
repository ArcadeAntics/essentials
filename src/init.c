#include <R_ext/Visibility.h>
#include "essentials.h"


static const R_CallMethodDef callRoutines[] = {
    {"as.scalar.logical"      , (DL_FUNC) &do_asscalarlogical       , 1},
    {"as.scalar.integer"      , (DL_FUNC) &do_asscalarinteger       , 1},
    {"as.scalar.real"         , (DL_FUNC) &do_asscalarreal          , 1},
    {"as.scalar.complex"      , (DL_FUNC) &do_asscalarcomplex       , 1},
    {"as.scalar.number"       , (DL_FUNC) &do_asscalarnumber        , 2},
    {"as.scalar.string"       , (DL_FUNC) &do_asscalarstring        , 1},
    {"as.scalar.raw"          , (DL_FUNC) &do_asscalarraw           , 1},

    {"as.scalar"              , (DL_FUNC) &do_asscalar              , 2},
    {"is.scalar"              , (DL_FUNC) &do_isscalar              , 2},
    {"as.numbers"             , (DL_FUNC) &as_numbers               , 2},

    // {"hypot"                  , (DL_FUNC) &hypot_vectorized         , 2},

    {"IDW"                    , (DL_FUNC) &IDW                      , 4},
    {"RK4"                    , (DL_FUNC) &RK4                      , 4},

    {"row.match.data.frame"   , (DL_FUNC) &do_rowmatchdataframe     , 5},

    {"match.type"             , (DL_FUNC) &do_match_type            , 2},

    {"isMissingArg"           , (DL_FUNC) &do_isMissingArg          , 2},

    {"tryExcept_on.exit_setup", (DL_FUNC) &do_tryExcept_onexit_setup, 2},

    {"do.while"               , (DL_FUNC) &do_dowhile               , 4},

    {"envvars"                , (DL_FUNC) &do_envvars               , 2},
    {"getEnvvar"              , (DL_FUNC) &do_getEnvvar             , 2},

    {"dgev", (DL_FUNC) &dgev, 5},
    {"pgev", (DL_FUNC) &pgev, 6},
    {"qgev", (DL_FUNC) &qgev, 6},
    {"rgev", (DL_FUNC) &rgev, 4},

    {"plapply"                , (DL_FUNC) &do_plapply               , 4},
    {"pvapply"                , (DL_FUNC) &do_pvapply               , 6},

    {"seq"                    , (DL_FUNC) &do_seq                   , 1},

    {"is.mfor.done"           , (DL_FUNC) &do_is_mfor_done          , 1},
    {"mfor"                   , (DL_FUNC) &do_mfor                  , 2},

    {"mkPROMISE"              , (DL_FUNC) &do_mkPROMISE             , 2},
    {"as.env"                 , (DL_FUNC) &do_as_env                , 3},
    {"f.str.old"              , (DL_FUNC) &do_f_str_old             , 4},
    {"f.str"                  , (DL_FUNC) &do_f_str                 , 3},

    {"do.expr"                , (DL_FUNC) &do_do_expr               , 2},

    {NULL, NULL, 0}
};


static const R_ExternalMethodDef externalRoutines[] = {
    {"hypot" , (DL_FUNC) &do_hypot , -1},
    {"phypot", (DL_FUNC) &do_phypot, -1},
    {NULL, NULL, 0}
};


void attribute_visible R_init_essentials(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callRoutines, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
