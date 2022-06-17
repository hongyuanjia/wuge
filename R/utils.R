split_char <- function(char) {
    if (!is.character(char)) {
        stop("Input character should be a characte vector.")
    } else if (anyNA(char)) {
        stop("Input character vector should not contain any NA.")
    }

    in_char <- strsplit(gsub("[ \t\r\n]", "", char), "", fixed = TRUE)
    n <- lengths(in_char)

    data.table::data.table(
        index = rep(seq_along(in_char), n),
        character = unlist(in_char, FALSE, FALSE)
    )
}

str_join <- function(..., sep = "") {
    paste0(..., collapse = sep)
}

str_split <- function(x, char = "") {
    strsplit(x, split = char, fixed = TRUE)
}

tree_chars <- function() {
    if (l10n_info()$`UTF-8`) {
        list(
            "v" = "\u2502",
            "h" = "\u2500",
            "l" = "\u2514",
            "j" = "\u2534",
            "u" = "\u1431"
        )
    } else {
        list(
            "v" = "|",
            "h" = "-",
            "l" = "+",
            "j" = "-",
            "u" = "^"
        )
    }
}
