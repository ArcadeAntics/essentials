asWindowsbasename <- function (path)
gsub("^ +|( |\\.)+$", "", chartr("\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\027\030\031\032\033\034\035\036\037\"*/:<>?\\|", "_________________________________________", path))


deaccent <- function (x)
chartr(old, new, x)
evalq(envir = environment(deaccent) <- new.env(), {
    table <- matrix(dimnames = list(NULL, c(
        "old"     , "new", "nickname"                              , "name"                                               )), data = c(

        "\u{00C0}", "A"  , "A with grave"                          , "LATIN CAPITAL LETTER A WITH GRAVE"                  ,
        "\u{00C1}", "A"  , "A with acute"                          , "LATIN CAPITAL LETTER A WITH ACUTE"                  ,
        "\u{00C2}", "A"  , "A with circumflex"                     , "LATIN CAPITAL LETTER A WITH CIRCUMFLEX"             ,
        "\u{00C3}", "A"  , "A with tilde"                          , "LATIN CAPITAL LETTER A WITH TILDE"                  ,
        "\u{00C4}", "A"  , "A with diaeresis"                      , "LATIN CAPITAL LETTER A WITH DIAERESIS"              ,
        "\u{00C5}", "A"  , "A with ring above"                     , "LATIN CAPITAL LETTER A WITH RING ABOVE"             ,
        "\u{00C7}", "C"  , "C with cedilla"                        , "LATIN CAPITAL LETTER C WITH CEDILLA"                ,
        "\u{00C8}", "E"  , "E with grave"                          , "LATIN CAPITAL LETTER E WITH GRAVE"                  ,
        "\u{00C9}", "E"  , "E with acute"                          , "LATIN CAPITAL LETTER E WITH ACUTE"                  ,
        "\u{00CA}", "E"  , "E with circumflex"                     , "LATIN CAPITAL LETTER E WITH CIRCUMFLEX"             ,
        "\u{00CB}", "E"  , "E with diaeresis"                      , "LATIN CAPITAL LETTER E WITH DIAERESIS"              ,
        "\u{00CC}", "I"  , "I with grave"                          , "LATIN CAPITAL LETTER I WITH GRAVE"                  ,
        "\u{00CD}", "I"  , "I with acute"                          , "LATIN CAPITAL LETTER I WITH ACUTE"                  ,
        "\u{00CE}", "I"  , "I with circumflex"                     , "LATIN CAPITAL LETTER I WITH CIRCUMFLEX"             ,
        "\u{00CF}", "I"  , "I with diaeresis"                      , "LATIN CAPITAL LETTER I WITH DIAERESIS"              ,
        "\u{00D0}", "D"  , "Eth"                                   , "LATIN CAPITAL LETTER ETH"                           ,
        "\u{00D1}", "N"  , "N with tilde"                          , "LATIN CAPITAL LETTER N WITH TILDE"                  ,
        "\u{00D2}", "O"  , "O with grave"                          , "LATIN CAPITAL LETTER O WITH GRAVE"                  ,
        "\u{00D3}", "O"  , "O with acute"                          , "LATIN CAPITAL LETTER O WITH ACUTE"                  ,
        "\u{00D4}", "O"  , "O with circumflex"                     , "LATIN CAPITAL LETTER O WITH CIRCUMFLEX"             ,
        "\u{00D5}", "O"  , "O with tilde"                          , "LATIN CAPITAL LETTER O WITH TILDE"                  ,
        "\u{00D6}", "O"  , "O with diaeresis"                      , "LATIN CAPITAL LETTER O WITH DIAERESIS"              ,
        "\u{00D8}", "O"  , "O with stroke"                         , "LATIN CAPITAL LETTER O WITH STROKE"                 ,
        "\u{00D9}", "U"  , "U with grave"                          , "LATIN CAPITAL LETTER U WITH GRAVE"                  ,
        "\u{00DA}", "U"  , "U with acute"                          , "LATIN CAPITAL LETTER U WITH ACUTE"                  ,
        "\u{00DB}", "U"  , "U with circumflex"                     , "LATIN CAPITAL LETTER U WITH CIRCUMFLEX"             ,
        "\u{00DC}", "U"  , "U with diaeresis"                      , "LATIN CAPITAL LETTER U WITH DIAERESIS"              ,
        "\u{00DD}", "Y"  , "Y with acute"                          , "LATIN CAPITAL LETTER Y WITH ACUTE"                  ,
        "\u{00DE}", "P"  , "Thorn"                                 , "LATIN CAPITAL LETTER THORN"                         ,
        "\u{00DF}", "s"  , "Sharp s"                               , "LATIN SMALL LETTER SHARP S"                         ,
        "\u{00E0}", "a"  , "a with grave"                          , "LATIN SMALL LETTER A WITH GRAVE"                    ,
        "\u{00E1}", "a"  , "a with acute"                          , "LATIN SMALL LETTER A WITH ACUTE"                    ,
        "\u{00E2}", "a"  , "a with circumflex"                     , "LATIN SMALL LETTER A WITH CIRCUMFLEX"               ,
        "\u{00E3}", "a"  , "a with tilde"                          , "LATIN SMALL LETTER A WITH TILDE"                    ,
        "\u{00E4}", "a"  , "a with diaeresis"                      , "LATIN SMALL LETTER A WITH DIAERESIS"                ,
        "\u{00E5}", "a"  , "a with ring above"                     , "LATIN SMALL LETTER A WITH RING ABOVE"               ,
        "\u{00E7}", "c"  , "c with cedilla"                        , "LATIN SMALL LETTER C WITH CEDILLA"                  ,
        "\u{00E8}", "e"  , "e with grave"                          , "LATIN SMALL LETTER E WITH GRAVE"                    ,
        "\u{00E9}", "e"  , "e with acute"                          , "LATIN SMALL LETTER E WITH ACUTE"                    ,
        "\u{00EA}", "e"  , "e with circumflex"                     , "LATIN SMALL LETTER E WITH CIRCUMFLEX"               ,
        "\u{00EB}", "e"  , "e with diaeresis"                      , "LATIN SMALL LETTER E WITH DIAERESIS"                ,
        "\u{00EC}", "i"  , "i with grave"                          , "LATIN SMALL LETTER I WITH GRAVE"                    ,
        "\u{00ED}", "i"  , "i with acute"                          , "LATIN SMALL LETTER I WITH ACUTE"                    ,
        "\u{00EE}", "i"  , "i with circumflex"                     , "LATIN SMALL LETTER I WITH CIRCUMFLEX"               ,
        "\u{00EF}", "i"  , "i with diaeresis"                      , "LATIN SMALL LETTER I WITH DIAERESIS"                ,
        "\u{00F0}", "d"  , "eth"                                   , "LATIN SMALL LETTER ETH"                             ,
        "\u{00F1}", "n"  , "n with tilde"                          , "LATIN SMALL LETTER N WITH TILDE"                    ,
        "\u{00F2}", "o"  , "o with grave"                          , "LATIN SMALL LETTER O WITH GRAVE"                    ,
        "\u{00F3}", "o"  , "o with acute"                          , "LATIN SMALL LETTER O WITH ACUTE"                    ,
        "\u{00F4}", "o"  , "o with circumflex"                     , "LATIN SMALL LETTER O WITH CIRCUMFLEX"               ,
        "\u{00F5}", "o"  , "o with tilde"                          , "LATIN SMALL LETTER O WITH TILDE"                    ,
        "\u{00F6}", "o"  , "o with diaeresis"                      , "LATIN SMALL LETTER O WITH DIAERESIS"                ,
        "\u{00F8}", "o"  , "o with stroke"                         , "LATIN SMALL LETTER O WITH STROKE"                   ,
        "\u{00F9}", "u"  , "u with grave"                          , "LATIN SMALL LETTER U WITH GRAVE"                    ,
        "\u{00FA}", "u"  , "u with acute"                          , "LATIN SMALL LETTER U WITH ACUTE"                    ,
        "\u{00FB}", "u"  , "u with circumflex"                     , "LATIN SMALL LETTER U WITH CIRCUMFLEX"               ,
        "\u{00FC}", "u"  , "u with diaeresis"                      , "LATIN SMALL LETTER U WITH DIAERESIS"                ,
        "\u{00FD}", "y"  , "y with acute"                          , "LATIN SMALL LETTER Y WITH ACUTE"                    ,
        "\u{00FE}", "p"  , "thorn"                                 , "LATIN SMALL LETTER THORN"                           ,
        "\u{00FF}", "y"  , "y with diaeresis"                      , "LATIN SMALL LETTER Y WITH DIAERESIS"                ,

        "\u{0363}", "a"  , "combining a"                           , "COMBINING LATIN SMALL LETTER A"                     ,
        "\u{0364}", "e"  , "combining e"                           , "COMBINING LATIN SMALL LETTER E"                     ,
        "\u{0365}", "i"  , "combining i"                           , "COMBINING LATIN SMALL LETTER I"                     ,
        "\u{0366}", "o"  , "combining o"                           , "COMBINING LATIN SMALL LETTER O"                     ,
        "\u{0367}", "u"  , "combining u"                           , "COMBINING LATIN SMALL LETTER U"                     ,
        "\u{0368}", "c"  , "combining c"                           , "COMBINING LATIN SMALL LETTER C"                     ,
        "\u{0369}", "d"  , "combining d"                           , "COMBINING LATIN SMALL LETTER D"                     ,
        "\u{036A}", "h"  , "combining h"                           , "COMBINING LATIN SMALL LETTER H"                     ,
        "\u{036B}", "m"  , "combining m"                           , "COMBINING LATIN SMALL LETTER M"                     ,
        "\u{036C}", "r"  , "combining r"                           , "COMBINING LATIN SMALL LETTER R"                     ,
        "\u{036D}", "t"  , "combining t"                           , "COMBINING LATIN SMALL LETTER T"                     ,
        "\u{036E}", "v"  , "combining v"                           , "COMBINING LATIN SMALL LETTER V"                     ,
        "\u{036F}", "x"  , "combining x"                           , "COMBINING LATIN SMALL LETTER X"                     ,

        "\u{0370}", "H"  , "Heta"                                  , "GREEK CAPITAL LETTER HETA"                          ,
        "\u{0371}", "h"  , "heta"                                  , "GREEK SMALL LETTER HETA"                            ,
        "\u{0376}", "F"  , "Pamphylia"                             , "GREEK CAPITAL LETTER PAMPHYLIA"                     ,
        "\u{0377}", "f"  , "pamphylia"                             , "GREEK SMALL LETTER PAMPHYLIA"                       ,
        "\u{037F}", "J"  , "Jot"                                   , "GREEK CAPITAL LETTER YOT"                           ,

        "\u{0386}", "A"  , "Alpha with tonos"                      , "GREEK CAPITAL LETTER ALPHA WITH TONOS"              ,
        "\u{0388}", "E"  , "Epsilon with tonos"                    , "GREEK CAPITAL LETTER EPSILON WITH TONOS"            ,
        "\u{0389}", "H"  , "Eta with tonos"                        , "GREEK CAPITAL LETTER ETA WITH TONOS"                ,
        "\u{038A}", "I"  , "Iota with tonos"                       , "GREEK CAPITAL LETTER IOTA WITH TONOS"               ,
        "\u{038C}", "O"  , "Omicron with tonos"                    , "GREEK CAPITAL LETTER OMICRON WITH TONOS"            ,
        "\u{038E}", "U"  , "Upsilon with tonos"                    , "GREEK CAPITAL LETTER UPSILON WITH TONOS"            ,
        "\u{038F}", "W"  , "Omega with tonos"                      , "GREEK CAPITAL LETTER OEMGA WITH TONOS"              ,

        "\u{0390}", "i"  , "iota with dialytika and tonos"         , "GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS"   ,

        "\u{0391}", "A"  , "Alpha"                                 , "GREEK CAPITAL LETTER ALPHA"                         ,
        "\u{0392}", "B"  , "Beta"                                  , "GREEK CAPITAL LETTER BETA"                          ,
        "\u{0393}", "C"  , "Gamma"                                 , "GREEK CAPITAL LETTER GAMMA"                         ,
        "\u{0394}", "D"  , "Delta"                                 , "GREEK CAPITAL LETTER DELTA"                         ,
        "\u{0395}", "E"  , "Epsilon"                               , "GREEK CAPITAL LETTER EPSILON"                       ,
        "\u{0396}", "Z"  , "Zeta"                                  , "GREEK CAPITAL LETTER ZETA"                          ,
        "\u{0397}", "H"  , "Eta"                                   , "GREEK CAPITAL LETTER ETA"                           ,
        "\u{0398}", "T"  , "Theta"                                 , "GREEK CAPITAL LETTER THETA"                         ,
        "\u{0399}", "I"  , "Iota"                                  , "GREEK CAPITAL LETTER IOTA"                          ,
        "\u{039A}", "K"  , "Kappa"                                 , "GREEK CAPITAL LETTER KAPPA"                         ,
        "\u{039B}", "L"  , "Lambda"                                , "GREEK CAPITAL LETTER LAMBDA"                        ,
        "\u{039C}", "M"  , "Mu"                                    , "GREEK CAPITAL LETTER MU"                            ,
        "\u{039D}", "N"  , "Nu"                                    , "GREEK CAPITAL LETTER NU"                            ,
        "\u{039E}", "X"  , "Xi"                                    , "GREEK CAPITAL LETTER XI"                            ,
        "\u{039F}", "O"  , "Omicron"                               , "GREEK CAPITAL LETTER OMICRON"                       ,

        "\u{03A0}", "P"  , "Pi"                                    , "GREEK CAPITAL LETTER PI"                            ,
        "\u{03A1}", "R"  , "Rho"                                   , "GREEK CAPITAL LETTER RHO"                           ,
        "\u{03A3}", "S"  , "Sigma"                                 , "GREEK CAPITAL LETTER SIGMA"                         ,
        "\u{03A4}", "T"  , "Tau"                                   , "GREEK CAPITAL LETTER TAU"                           ,
        "\u{03A5}", "U"  , "Upsilon"                               , "GREEK CAPITAL LETTER UPSILON"                       ,
        "\u{03A6}", "F"  , "Phi"                                   , "GREEK CAPITAL LETTER PHI"                           ,
        "\u{03A7}", "C"  , "Chi"                                   , "GREEK CAPITAL LETTER CHI"                           ,
        "\u{03A8}", "Y"  , "Psi"                                   , "GREEK CAPITAL LETTER PSI"                           ,
        "\u{03A9}", "W"  , "Omega"                                 , "GREEK CAPITAL LETTER OMEGA"                         ,

        "\u{03AA}", "I"  , "Iota with dialytika"                   , "GREEK CAPITAL LETTER IOTA WITH DIALYTIKA"           ,
        "\u{03AB}", "U"  , "Upsilon with dialytika"                , "GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA"        ,
        "\u{03AC}", "a"  , "alpha with tonos"                      , "GREEK SMALL LETTER ALPHA WITH TONOS"                ,
        "\u{03AD}", "e"  , "epsilon with tonos"                    , "GREEK SMALL LETTER EPSILON WITH TONOS"              ,
        "\u{03AE}", "h"  , "eta with tonos"                        , "GREEK SMALL LETTER ETA WITH TONOS"                  ,
        "\u{03AF}", "i"  , "iota with tonos"                       , "GREEK SMALL LETTER IOTA WITH TONOS"                 ,

        "\u{03B0}", "u"  , "upsilon with dialytika and tonos"      , "GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS",
        "\u{03B1}", "a"  , "alpha"                                 , "GREEK SMALL LETTER ALPHA"                           ,
        "\u{03B2}", "b"  , "beta"                                  , "GREEK SMALL LETTER BETA"                            ,
        "\u{03B3}", "c"  , "gamma"                                 , "GREEK SMALL LETTER GAMMA"                           ,
        "\u{03B4}", "d"  , "delta"                                 , "GREEK SMALL LETTER DELTA"                           ,
        "\u{03B5}", "e"  , "epsilon"                               , "GREEK SMALL LETTER EPSILON"                         ,
        "\u{03B6}", "z"  , "zeta"                                  , "GREEK SMALL LETTER ZETA"                            ,
        "\u{03B7}", "h"  , "eta"                                   , "GREEK SMALL LETTER ETA"                             ,
        "\u{03B8}", "t"  , "theta"                                 , "GREEK SMALL LETTER THETA"                           ,
        "\u{03B9}", "i"  , "iota"                                  , "GREEK SMALL LETTER IOTA"                            ,
        "\u{03BA}", "k"  , "kappa"                                 , "GREEK SMALL LETTER KAPPA"                           ,
        "\u{03BB}", "l"  , "lambda"                                , "GREEK SMALL LETTER LAMBDA"                          ,
        "\u{03BC}", "m"  , "mu"                                    , "GREEK SMALL LETTER MU"                              ,
        "\u{03BD}", "n"  , "nu"                                    , "GREEK SMALL LETTER NU"                              ,
        "\u{03BE}", "x"  , "xi"                                    , "GREEK SMALL LETTER XI"                              ,
        "\u{03BF}", "o"  , "omicron"                               , "GREEK SMALL LETTER OMICRON"                         ,
        "\u{03C0}", "p"  , "pi"                                    , "GREEK SMALL LETTER PI"                              ,
        "\u{03C1}", "r"  , "rho"                                   , "GREEK SMALL LETTER RHO"                             ,
        "\u{03C2}", "s"  , "final sigma"                           , "GREEK SMALL LETTER FINAL SIGMA"                     ,
        "\u{03C3}", "s"  , "sigma"                                 , "GREEK SMALL LETTER SIGMA"                           ,
        "\u{03C4}", "t"  , "tau"                                   , "GREEK SMALL LETTER TAU"                             ,
        "\u{03C5}", "u"  , "upsilon"                               , "GREEK SMALL LETTER UPSILON"                         ,
        "\u{03C6}", "f"  , "phi"                                   , "GREEK SMALL LETTER PHI"                             ,
        "\u{03C7}", "c"  , "chi"                                   , "GREEK SMALL LETTER CHI"                             ,
        "\u{03C8}", "y"  , "psi"                                   , "GREEK SMALL LETTER PSI"                             ,
        "\u{03C9}", "w"  , "omega"                                 , "GREEK SMALL LETTER OMEGA"                           ,

        "\u{03CA}", "i"  , "iota with dialytika"                   , "GREEK SMALL LETTER IOTA WITH DIALYTIKA"             ,
        "\u{03CB}", "u"  , "upsilon with dialytika"                , "GREEK SMALL LETTER UPSILON WITH DIALYTIKA"          ,
        "\u{03CC}", "o"  , "omicron with tonos"                    , "GREEK SMALL LETTER OMICRON WITH TONOS"              ,
        "\u{03CD}", "u"  , "upsilon with tonos"                    , "GREEK SMALL LETTER UPSILON WITH TONOS"              ,
        "\u{03CE}", "w"  , "omega with tonos"                      , "GREEK SMALL LETTER OMEGA WITH TONOS"                ,
        "\u{03CF}", "K"  , "Kai symbol"                            , "GREEK CAPITAL KAI SYMBOL"                           ,

        "\u{03D0}", "b"  , "beta symbol"                           , "GREEK BETA SYMBOL"                                  ,
        "\u{03D1}", "t"  , "theta symbol"                          , "GREEK THETA SYMBOL"                                 ,
        "\u{03D2}", "U"  , "Upsilon with hook symbol"              , "GREEK UPSILON WITH HOOK SYMBOL"                     ,
        "\u{03D3}", "U"  , "Upsilon with acute and hook symbol"    , "GREEK UPSILON WITH ACUTE AND HOOK SYMBOL"           ,
        "\u{03D4}", "U"  , "Upsilon with diaeresis and hook symbol", "GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL"       ,
        "\u{03D5}", "F"  , "Phi symbol"                            , "GREEK PHI SYMBOL"                                   ,
        "\u{03D6}", "w"  , "Pi symbol"                             , "GREEK PI SYMBOL"                                    ,
        "\u{03D7}", "k"  , "kai symbol"                            , "GREEK KAI SYMBOL"                                   ,
        "\u{03D8}", "K"  , "Koppa"                                 , "GREEK LETTER ARCHAIC KOPPA"                         ,
        "\u{03D9}", "k"  , "koppa"                                 , "GREEK SMALL LETTER KOPPA"                           ,

        "\u{2200}", "A"  , "for all"                               , "FOR ALL"

    ), ncol = 4, byrow = TRUE)
    old <- paste(table[, "old"], collapse = "")
    new <- paste(table[, "new"], collapse = "")
    rm(table)
})


portable.basename <- function (path, upper = FALSE)
{
    path <- deaccent(path)
    path <- gsub(fancy.hyphens, "-", path)
    # path <- chartr("$", "s", path)


    # replace "(text)" with " - text - "
    # same with
    path <- gsub("\\(([^)]+)\\)", "_\\1_", path)
    path <- gsub("\\[([^]]+)\\]", "_\\1_", path)
    path <- gsub(":[ \t]", "_", path)
    path <- gsub(":", "-", path, fixed = TRUE)
    path <- gsub("[ \t]-[ \t]"  , "_", path)
    path <- gsub("[ \t]*&+[ \t]*", " and ", path)
    # path <- gsub(",", "_", path, fixed = TRUE)
    # path <- gsub("/", "_", path, fixed = TRUE)
    # path <- gsub("+", "-", path, fixed = TRUE)
    path <- chartr(",/+", "___", path)
    path <- gsub(" +"   , "-", path)


    upper <- as.scalar.logical(upper)
    if (is.na(upper))
        path <- gsub("[^0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.-]", "", path)
    else if (upper) {
        path <- toupper(path)
        path <- gsub("[^0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_.-]", "", path)
    }
    else {
        path <- tolower(path)
        path <- gsub("[^0123456789abcdefghijklmnopqrstuvwxyz_.-]", "", path)
    }


    # remove - from start and _.- from end
    path <- gsub("(^-+)|([_.-]+$)", "", path)
    # condense multiple - into single -
    path <- gsub("-{2,}", "-", path)
    #
    path <- gsub("[_-]{2,}", "_", path)
    path
}
evalq(envir = environment(portable.basename) <- new.env(), {
    hyphens.table <- matrix(dimnames = list(NULL, c(
        "old"     , "new", "nickname"              , "name"                           )), data = c(

        "\u{00AD}", "-"  , "soft hyphen"           , "SOFT HYPHEN"                    ,
        "\u{058A}", "-"  , "Armenian hyphen"       , "ARMENIAN HYPHEN"                ,
        "\u{05BE}", "-"  , "Hebrew hyphen"         , "HEBREW PUNCTUATION MAQAF"       ,
        "\u{1400}", "-"  , "Canadian hyphen"       , "CANADIAN SYLLABICS HYPHEN"      ,
        "\u{1806}", "-"  , "Mongolian hyphen"      , "MONGOLIAN TODO SOFT HYPHEN"     ,
        "\u{2010}", "-"  , "hyphen"                , "HYPHEN"                         ,
        "\u{2011}", "-"  , "non-breaking hyphen"   , "NON-BREAKING HYPHEN"            ,
        "\u{2012}", "-"  , "figure hyphen"         , "FIGURE DASH"                    ,
        "\u{2013}", "-"  , "en dash"               , "EN DASH"                        ,
        "\u{2014}", "-"  , "em dash"               , "EM DASH"                        ,
        "\u{2015}", "-"  , "horizontal bar"        , "HORIZONTAL BAR"                 ,
        "\u{2212}", "-"  , "minus sign"            , "MINUS SIGN"                     ,
        "\u{2E3A}", "-"  , "two-em dash"           , "TWO-EM DASH"                    ,
        "\u{2E3B}", "-"  , "three-em dash"         , "THREE-EM DASH"                  ,
        "\u{30A0}", "-"  , "katakana hyphen"       , "KATAKANA-HIRAGANA DOUBLE HYPHEN",
        "\u{FE58}", "-"  , "small em dash"         , "SMALL EM DASH"                  ,
        "\u{FE63}", "-"  , "small hyphen-minus"    , "SMALL HYPHEN-MINUS"             ,
        "\u{FF0D}", "-"  , "fullwidth hyphen-minus", "FULLWIDTH HYPHEN-MINUS"
    ), ncol = 4, byrow = TRUE)
    fancy.hyphens <- paste(hyphens.table[, "old"], collapse = "|")
    rm(hyphens.table)
})
