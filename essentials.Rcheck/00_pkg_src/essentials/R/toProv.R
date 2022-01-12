Provs <- c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON",
    "PE", "QC", "SK", "YT")


Provs2 <- Provs
Provs2[Provs2 == "NL"] <- "NF"


Provinces <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick",
    "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories",
    "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan",
    "Yukon")


prepare4provmatch <- function (x)
{
    attributes(x) <- NULL
    x <- toupper(x)
    x <- gsub("\u{00E9}|\u{00C9}|\u{00C3}\u{00A9}|\u{00C3}\u{0089}", "E", x)
    x <- gsub("\u{00EE}|\u{00CE}|\u{00C3}\u{00AE}|\u{00C3}\u{008E}", "I", x)
    gsub("[[:space:][:punct:]]", "", x)
}


ProvTable <- local({
    fun <- function(x, y) {
        nm <- names(y)
        x[nm] <- .mapply(base::c, list(x[nm], y[nm]), NULL)
        x
    }
    value <- vector("list", length(Provs))
    names(value) <- Provs


    # names (english)
    value <- fun(value, `names<-`(Provinces, Provs))


    # postal abbreviations
    value <- fun(value, `names<-`(Provs, Provs))


    # traditional abbreviations (english)
    value <- fun(value, list(
        AB = "Alta.",
        BC = "B.C.",
        MB = "Man.",
        NB = "N.B.",
        NL = "N.L.",
        NS = "N.S.",
        NT = "N.W.T.",
        NU = "Nvt.",
        ON = "Ont.",
        PE = "P.E.I.",
        QC = "Que.",
        SK = "Sask.",
        YT = "Yuk."
    ))


    # traditional abbreviations (french)
    value <- fun(value, list(
        AB = "Alb.",
        BC = "C.-B.",
        MB = "Man.",
        NB = "N.-B.",
        NL = "T.-N.-L.",
        NS = "N.-\u{00C9}.",
        NT = "T.N.-O.",


        # # this overlaps with postal abbr. "NT" for Northwest Territories
        # NU = "Nt",


        ON = "Ont.",
        PE = "\u{00CE}.-P.-\u{00C9}.",
        QC = "Qc",
        SK = "Sask.",
        YT = "Yn"
    ))


    # notes
    value <- fun(value, list(
        BC = c("C.-B.", "Colombie Britannique"),
        NB = c("N.-B.", "Nouveau-Brunswick"),
        NL = c("Nfld.", "T.-N.", "Newfoundland",
            "Terre-Neuve", "T.-.N.-L.", "Terre-Neuve-et-Labrador",
            "NF", "LB"),
        NS = c("N.-\u{00C9}.", "Nouvelle-\u{00C9}cosse"),
        NT = c("T.N.-O.", "Territoires du Nord-Ouest"),
        ON = "O",
        PE = c("\u{00CE}.-P.-\u{00C9}.", "\u{00CE}le du Prince-\u{00C9}douard"),
        QC = c("P.Q.", "Province du Qu\u{00E9}bec", "PQ",
            "QU", "QB"),
        YT = "YK"
    ))


    # others
    value <- fun(value, list(
        NL = c("Newfoundland & Labrador", "Terre-Neuve & Labrador"),
        NT = "Territoires Nord-Ouest",
        PE = c("PEI", "\u{00CE}le Prince-\u{00C9}douard"),
        QC = c("Qu\u{00E9}bec", "Province Qu\u{00E9}bec"),
        YT = "Yukon Territory"
    ))


    # mistaken
    value <- fun(value, list(
        AB = "AL",
        ON = "0N"
    ))


    value <- c(value, list(CA = c("CA", "Canada")))
    lapply(value, function(xx) unique(prepare4provmatch(xx)))
})


toProv <- function (x)
{
    if (missing(x))
        return(Provs)
    if (is.factor(x) && length(levels(x)) < length(x))
        return(toProv(levels(x))[x])
    if (!is.character(x))
        x <- as.character(x)
    table <- ProvTable
    ids <- rep(seq_along(table), lengths(table))
    value <- names(table)[ids[pmatch(prepare4provmatch(x), unlist(table), duplicates.ok = TRUE)]]
    attributes(value) <- attributes(x)
    return(unclass(value))
}


toProv2 <- function (x)
{
    if (missing(x))
        return(Provs2)
    value <- toProv(x)
    value[value == "NL"] <- "NF"
    return(value)
}


toProvince <- function (x)
{
    if (missing(x))
        return(Provinces)
    c(Provinces, "Canada")[match(toProv(x), c(Provs, "CA"))]
}
