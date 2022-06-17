#' Convert Simplified Chinese to Traditional Chinese
#'
#' `sim2trad()` uses the dictionary `STCharacter.txt` distributed via
#' [`OpenCC`](https://opencc.byvoid.com/) to convert Simplified Chinese to
#' Traditional Chinese.
#'
#' @param char A character vector of Simplified Chinese characters.
#'
#' @param all If `TRUE`, all possible match results of Traditional Chinese
#'        characters are returned. If `FALSE`, only the first match is returned.
#'        Default: `FALSE`.
#'
#' @return
#' If `all` is `FALSE`, a character vector with the same length as input `char`.
#' Otherwise, a list of character vector with the same length as input `char`.
#' The returned character vector or list is always named using input `char`.
#'
#' @note
#' Currently, {wuge} package did not use `OpenCC` library. Instead, a simple
#' character-level mapping is used.
#'
#' @examples
#' sim2trad(c("千万", "鼠标"))
#' sim2trad(c("千万", "鼠标"), all = TRUE)
#'
#' @export
sim2trad <- function(char, all = FALSE) {
    input <- split_char(char)
    match <- dict_conv()[input, on = c("simplified" = "character")]

    # NOTE: to make CRAN checks happy
    simplified <- NULL
    traditional <- NULL

    match[is.na(traditional), traditional := simplified]

    if (!all) {
        match[, traditional := substring(traditional, 1L, 1L)]
        match <- match[,
            list(traditional = str_join(traditional)),
            "index"
        ]
    } else {
        match[, traditional := str_split(traditional)]
        match <- match[, {
            n <- max(lengths(traditional))
            if (n > 1L) {
                traditional <- Map(rep_len, x = traditional, length.out = n)
            }
            tra <- lapply(do.call(Map, c(f = c, traditional)), paste, collapse = "")
            list(traditional = list(unlist(tra, FALSE, FALSE)))
        }, "index"]
    }

    res <- match$traditional
    names(res) <- char

    res
}
