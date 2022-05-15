parser <- essentials::ArgumentParser()
parser$add.argument("--foo", help = "foo help")
parser$print.help()


parser <- essentials::ArgumentParser(program = "myprogram")
parser$print.help()


parser <- essentials::ArgumentParser(program = "myprogram")
parser$add.argument("--foo", help = "foo of the %{sQuote(PROGRAM)}s program")
parser$print.help()


parser <- essentials::ArgumentParser(program = "PROGRAM")
parser$add.argument("--foo", nargs = "?", help = "foo help")
parser$add.argument("bar", nargs = "+", help = "bar help")
parser$print.help()


parser <- essentials::ArgumentParser(description = "A foo that bars")
parser$print.help()


parser <- essentials::ArgumentParser(
    description = "A foo that bars",
    epilogue = "And that's how you'd foo a bar"
)
parser$print.help()







parser <- essentials::ArgumentParser(
    program = "PROGRAM",
    description = "this description
        was indented weird
            but that is okay",
    epilogue = "
            likewise for this epilogue whose whitespace wil
        be cleaned up and whose words will be wrapped
        across a couple lines"
)
parser$print.help()


parser <- essentials::ArgumentParser(
    program = "PROGRAM",
    wrap.description = FALSE,
    description = essentials::dedent("
        Please do not mess up this text!
        --------------------------------
            I have indented it
            exactly the way
            I want it
            "))
parser$print.help()
