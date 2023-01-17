char_data <- function(char) {
    match <- dict_char()[split_char(char), on = "character"]
    data.table::setcolorder(match, "index")
    data.table::setindexv(match, "character")

    if (anyNA(match$stroke)) {
        # NOTE: to make CRAN checks happy
        stroke <- .N <- .BY <- NULL
        info <- match[,
            sprintf("[%i] \"%s\"\n%s%s\n%s%s Unsupported character found here.",
                .BY$index, str_join(character),
                strrep(" ", 4L + nchar(.BY$index)),
                {
                    spc <- rep(" ", .N)
                    spc[is.na(stroke)] <- tree_chars()$u
                    str_join(spc)
                },
                strrep(" ", 4L + nchar(.BY$index)),
                {
                    loc <- which(is.na(stroke))
                    ln <- rep(" ", max(loc) + 4L)
                    ln[loc[1L]] <- tree_chars()$l
                    ln[loc[-1L]] <- tree_chars()$j
                    ln[setdiff(seq(loc[1L] + 1L, max(loc) + 4L), loc)] <- tree_chars()$h
                    str_join(ln)
                }
            ),
            "index"
        ]$V1

        stop(sprintf(
            "Unsupported character(s) found:\n%s", str_join(info, sep = "\n")
        ))
    }

    match
}

#' Get stroke number of Chinese characters
#'
#' `get_stroke()` returns the stroke number of input Chinese characters. Both
#' Simplified Chinese and Traditional Chinese are supported.
#'
#' @param char A character vector of Chinese characters.
#'
#' @return
#' An integer vector with the same length as input `char`.
#' The returned integer vector is always named using input `char`.
#'
#' @examples
#' stroke(c("千万", "鼠标"))
#'
#' @export
stroke <- function(char) {
    # NOTE: to make CRAN checks happy
    stroke <- NULL
    match <- char_data(char)[, by = "index",
        list(character = str_join(character), stroke = sum(stroke))
    ]

    res <- match$stroke
    names(res) <- match$character

    res
}

#' Get pinyin of Chinese characters
#'
#' `get_pinyin()` returns the pinyin of input Chinese characters. Both
#' Simplified Chinese and Traditional Chinese are supported.
#'
#' @param char A character vector of Chinese characters.
#'
#' @return
#' A character vector with the same length as input `char`. The returned integer
#' vector is always named using input `char`.
#'
#' @note
#' Currently, polyphonics are not supported.
#'
#' @examples
#' pinyin(c("平安", "行"))
#'
#' @export
pinyin <- function(char) {
    match <- char_data(char)
    data.table::set(match, NULL, "pinyin", gsub("\\|.+$", "", match$pinyin))

    # NOTE: to make CRAN checks happy
    pinyin <- NULL
    match <- match[, by = "index", list(
        character = str_join(character), pinyin = str_join(pinyin, sep = " ")
    )]

    res <- match$pinyin
    names(res) <- match$character

    res
}
