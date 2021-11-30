\name{toProv}
\alias{toProv}
\alias{toProv2}
\alias{toProvince}
\title{
Convert the Provinces and Territories of Canada to their Names or Postal
Abbreviations
}
\description{
This is mostly intended to solve issues where the names or postal abbreviations
may be written incorrectly (in one of the alternate forms listed below).
}
\usage{
toProv(x)
toProv2(x)

toProvince(x)
}
\arguments{
  \item{x}{
character vector. Province and territory names or postal abbreviations.
  }
}
\details{
The following are the names and postal abbreviations of each province and
territory, as well as their accepted alternate forms.
\describe{
  \item{\code{Ontario}, \code{ON}}{
\code{Ont.}, \code{O}, \code{0N}
  }
  \item{\code{Quebec}, \code{QC}}{
\code{Québec}, \code{Province du Québec}, \code{Province Québec}, \code{Que.},
\code{Qc}, \code{P.Q.}, \code{PQ}, \code{QU}, \code{QB}
  }
  \item{\code{Nova Scotia}, \code{NS}}{
\code{Nouvelle-Écosse}, \code{N.S.}, \code{N.-É.}
  }
  \item{\code{New Brunswick}, \code{NB}}{
\code{Nouveau-Brunswick}, \code{N.B.}, \code{N.-B.}
  }
  \item{\code{Manitoba}, \code{MB}}{
\code{Man.}
  }
  \item{\code{British Columbia}, \code{BC}}{
\code{Colombie-Britannique}, \code{B.C.}, \code{C.-B.}
  }
  \item{\code{Prince Edward Island}, \code{PE}}{
\code{P.E.I.}, \code{Î.-P.-É.}, \code{PEI}, \code{Île du Prince-Édouard},
\code{Île Prince-Édouard}
  }
  \item{\code{Saskatchewan}, \code{SK}}{
\code{Sask.}
  }
  \item{\code{Alberta}, \code{AB}}{
\code{Alta.}, \code{Alb.} \code{AL}
  }
  \item{\code{Newfoundland and Labrador}, \code{NL}}{
\code{Newfoundland & Labrador}, \code{Newfoundland},
\code{Terre-Neuve-et-Labrador}, \code{Terre-Neuve & Labrador},
\code{Terre-Neuve}, \code{N.L.}, \code{T.-N.-L.}, \code{Nfld.}, \code{T.-N.},
\code{NF}, \code{LB}
  }
  \item{\code{Northwest Territories}, \code{NT}}{
\code{Territoires du Nord-Ouest}, \code{Territoires Nord-Ouest},
\code{N.W.T.}, \code{T.N.-O.}
  }
  \item{\code{Yukon}, \code{YT}}{
\code{Yukon Territory}, \code{Yuk.}, \code{Yn}, \code{YK}
  }
  \item{\code{Nunavut}, \code{NU}}{
\code{Nvt.}
  }
}
Additionally, the name and postal abbreviation for \code{Canada} are
\code{Canada} and \code{CA}. These will rarely be used, but are available if
need be. \code{x} will be partially matched with the above names, postal
abbreviations, and alternate forms (ignoring whitespace and character case).
}
\value{
character vector of the same length and with the same attributes as \code{x}
(after possible coercion to character) besides \code{class}.

for \code{toProv2}, elements of \code{toProv} equal to \code{"NL"} will return
as \code{"NF"}.
}
\examples{
x <- c(
    "Ontario"                  , "ON", "0N",


    # alternate forms
    "Quebec"                   , "QC", "Qu\\u{00E9}bec",


    # case insensitive
    "Nova Scotia"              , "NS", "nova scotia",
    "New Brunswick"            , "NB",
    "Manitoba"                 , "MB",
    "British Columbia"         , "BC",
    "Prince Edward Island"     , "PE", "PEI",


    # partial matching
    "Saskatchewan"             , "SK", "Sask",
    "Alberta"                  , "AB", "AL",
    "Newfoundland and Labrador", "NL", "Newfoundland & Labrador", "NF",
    "Northwest Territories"    , "NT",
    "Yukon"                    , "YT", "Yukon Territory", "YK",
    "Nunavut"                  , "NU"
)
cbind(Original = x, `Postal Abbr` = toProv(x), Name = toProvince(x))
}
