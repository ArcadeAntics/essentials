essentials:::check.this(  ## essentials
    special = TRUE,

    check = FALSE, as.cran = TRUE,

    chdir = TRUE
)


essentials::delayedAssign2("x", compiler::compile(quote(5 + 6)))
this.path:::.PRINFO(x)
x
this.path:::.PRINFO(x)


jswitch("a",
    case("a") := {
        writeLines("a")
        break
    },
    icase("b") := {
        writeLines("b or B")
        break
    },
    case("c") := {
        writeLines("c")
    },
    recase("d") := {
        writeLines("contains d (or is c)")
        break
    },
    default := {
        writeLines("default")
    }
)


jswitch(1,
    case(1) := {
        writeLines("\n1\n")
    },
    case(5) := {
        writeLines("\n5 (or 1)\n")
        break
    },
    default := {
        writeLines("\ndefault\n")
    }
)



fun <- function (arg)
{
    jswitch(arg,
        case("foo"),
        case("bar") := {
            writeLines("\n\"foo\" or \"bar\" (case sensitive)\n")
            break
        },
        icase("pi") := {
            writeLines("\n\"pi\" or \"Pi\" or \"pI\" or \"PI\" (case insensitive)\n")
            break
        },
        recase("^D") := {
            writeLines("\nSomething that starts with \"D\" (case sensitive)\n")
            break
        },
        recase("^E", ignore.case = TRUE) := {
            writeLines("\nSomething that starts with \"E\" (case insensitive)\n")
            break
        },
        case("1") := {
            writeLines("\n\"1\"\n")
            # break omitted on purpose
        },
        case("2") := {
            writeLines("\n\"2\" (or \"1\")\n")
            break
        },
        default := {
            writeLines("\ndefault\n")
            break
        }
    )
}


fun("foo")
fun("FOO")
fun("bar")
fun("BAR")
fun("pi")
fun("Pi")
fun("pI")
fun("PI")
fun("Dentist")
fun("dentist")
fun("Entymology")
fun("entymology")
fun("1")
fun("2")
fun("none of the above")
