seq2 <- function (from = NULL, to = NULL, by = NULL, length.out = NULL,
    along.with = NULL, endpoint = TRUE)
{
    r"{
    seq {essentials}                                                 R Documentation

    Sequence Generation



    Description:

    Generate regular sequences.



    Usage:

    seq2(from = NULL, to = NULL, by = NULL, length.out = NULL,
        along.with = NULL, endpoint = TRUE)



    Arguments:

    from, to

        the starting and (maximal) end values of the sequence. Of length 1 unless
        just from is supplied.

    by

        number: increment of the sequence.

    length.out

        desired length of the sequence. A negative number will return a zero-length
        array, while a non-negative number will be rounded up if fractional.

    along.with

        take the length from the length of this argument.

    endpoint

        if the last index of the result is equal to `to`, should it be kept?



    Details:

    A negative `length.out` will return a zero-length array, unlike `seq` where an
    error would be thrown.

    Additionally, specifying `to - from` and `by` of opposite signs will return a
    zero-length array, unlike `seq` where an error would be thrown.



    Value:

    A vector of type "integer" or "double".



    See Also:

    `seq`



    Examples:

    essentials:::seq2(from =  26, endpoint = TRUE )
    essentials:::seq2(from =  26, endpoint = FALSE)
    essentials:::seq2(from =   0, endpoint = TRUE )
    essentials:::seq2(from =   0, endpoint = FALSE)
    essentials:::seq2(from = -26, endpoint = TRUE )
    essentials:::seq2(from = -26, endpoint = FALSE)


    essentials:::seq2(from = letters, endpoint = TRUE )
    essentials:::seq2(from = letters, endpoint = FALSE)


    essentials:::seq2(to =  26, endpoint = TRUE )
    essentials:::seq2(to =  26, endpoint = FALSE)
    essentials:::seq2(to = -24, endpoint = TRUE )
    essentials:::seq2(to = -24, endpoint = FALSE)


    essentials:::seq2(by =  26, endpoint = TRUE )
    essentials:::seq2(by =  26, endpoint = FALSE)
    essentials:::seq2(by =   0, endpoint = TRUE )
    essentials:::seq2(by =   0, endpoint = FALSE)
    essentials:::seq2(by = -26, endpoint = TRUE )
    essentials:::seq2(by = -26, endpoint = FALSE)


    essentials:::seq2(length.out =  26, endpoint = TRUE )
    essentials:::seq2(length.out =  26, endpoint = FALSE)
    essentials:::seq2(length.out =   0, endpoint = TRUE )
    essentials:::seq2(length.out =   0, endpoint = FALSE)
    essentials:::seq2(length.out = -26, endpoint = TRUE )
    essentials:::seq2(length.out = -26, endpoint = FALSE)


    essentials:::seq2(along.with = letters, endpoint = TRUE )
    essentials:::seq2(along.with = letters, endpoint = FALSE)





    essentials:::seq2(from =  1, to = 26, endpoint = TRUE )
    essentials:::seq2(from =  1, to = 26, endpoint = FALSE)
    essentials:::seq2(from = 26, to =  1, endpoint = TRUE )
    essentials:::seq2(from = 26, to =  1, endpoint = FALSE)


    essentials:::seq2(from =  26, by =  1, endpoint = TRUE )
    essentials:::seq2(from =  26, by =  1, endpoint = FALSE)
    essentials:::seq2(from = -24, by =  1, endpoint = TRUE )
    essentials:::seq2(from = -24, by =  1, endpoint = FALSE)
    essentials:::seq2(from =  26, by = -1, endpoint = TRUE )
    essentials:::seq2(from =  26, by = -1, endpoint = FALSE)
    essentials:::seq2(from = -24, by = -1, endpoint = TRUE )
    essentials:::seq2(from = -24, by = -1, endpoint = FALSE)


    essentials:::seq2(from = 1, length.out = 26, endpoint = TRUE )
    essentials:::seq2(from = 1, length.out = 26, endpoint = FALSE)


    essentials:::seq2(from = 1, along.with = letters, endpoint = TRUE )
    essentials:::seq2(from = 1, along.with = letters, endpoint = FALSE)


    essentials:::seq2(to =  26, by =  1, endpoint = TRUE )
    essentials:::seq2(to =  26, by =  1, endpoint = FALSE)
    essentials:::seq2(to = -24, by =  1, endpoint = TRUE )
    essentials:::seq2(to = -24, by =  1, endpoint = FALSE)
    essentials:::seq2(to =  26, by = -1, endpoint = TRUE )
    essentials:::seq2(to =  26, by = -1, endpoint = FALSE)
    essentials:::seq2(to = -24, by = -1, endpoint = TRUE )
    essentials:::seq2(to = -24, by = -1, endpoint = FALSE)


    essentials:::seq2(to = 26, length.out = 26, endpoint = TRUE )
    essentials:::seq2(to = 26, length.out = 26, endpoint = FALSE)


    essentials:::seq2(to = 26, along.with = letters, endpoint = TRUE )
    essentials:::seq2(to = 26, along.with = letters, endpoint = FALSE)


    essentials:::seq2(by = 3, length.out = 26, endpoint = TRUE )
    essentials:::seq2(by = 3, length.out = 26, endpoint = FALSE)


    essentials:::seq2(by = 3, along.with = letters, endpoint = TRUE )
    essentials:::seq2(by = 3, along.with = letters, endpoint = FALSE)





    essentials:::seq2(from = -25, to = 25, by = 2, endpoint = TRUE )
    essentials:::seq2(from = -25, to = 25, by = 2, endpoint = FALSE)


    essentials:::seq2(from = 0, to = 25, length.out = 26, endpoint = TRUE )
    essentials:::seq2(from = 0, to = 25, length.out = 26, endpoint = FALSE)


    essentials:::seq2(from = 0, to = 25, along.with = letters, endpoint = TRUE )
    essentials:::seq2(from = 0, to = 25, along.with = letters, endpoint = FALSE)


    essentials:::seq2(from = 0, by = 2, length.out = 26, endpoint = TRUE )
    essentials:::seq2(from = 0, by = 2, length.out = 26, endpoint = FALSE)


    essentials:::seq2(from = 0, by = 2, along.with = letters, endpoint = TRUE )
    essentials:::seq2(from = 0, by = 2, along.with = letters, endpoint = FALSE)


    essentials:::seq2(to = 50, by = 2, length.out = 26, endpoint = TRUE )
    essentials:::seq2(to = 50, by = 2, length.out = 26, endpoint = FALSE)


    essentials:::seq2(to = 50, by = 2, along.with = letters, endpoint = TRUE )
    essentials:::seq2(to = 50, by = 2, along.with = letters, endpoint = FALSE)





    essentials:::seq2(length.out = 5)


    essentials:::seq2(0, 60, length.out = 21, endpoint = TRUE )
    essentials:::seq2(0, 60, length.out = 21, endpoint = FALSE)
    essentials:::seq2(0, 60, length.out = 20, endpoint = TRUE )
    essentials:::seq2(0, 60, length.out = 20, endpoint = FALSE)


    essentials:::seq2(60, length.out = 20, endpoint = FALSE)

    }"

    args <- list(from, to, by, length.out, along.with, endpoint)
    missings <- c(missing(from), missing(to), missing(by), missing(length.out), missing(along.with))
    .Call(C_seq, args, missings)
}
