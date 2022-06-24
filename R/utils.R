psum <- function(x) {
    ind <- seq_len(length(x))
    init <- .subset2(x, 1L)
    for (i in .subset(ind, -1L)) {
        init <- init + .subset2(x, i)
    }
    init
}

is_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x)
}

is_choice <- function(x, choices) {
    is_string(x) && !is.na(data.table::chmatch(x, choices))
}

assert_choice <- function(x, choices, null_ok = TRUE) {
    nm <- deparse(substitute(x))
    if (is.null(x)) {
        if (null_ok) return(x)

        stop(sprintf(
            "`%s` should be a single string , not `NULL`.",
            nm, min
        ))
    }

    if (!is_choice(x, choices)) {
        stop(sprintf(
            "`%s` should be element of set [%s] but is `%s`.",
            nm, str_join(sQuote(choices), sep = ", "), x
        ))
    }

    as.integer(x)
}

is_count <- function(x, min = 0L) {
    is.numeric(x) && length(x) == 1L && !is.na(x) &&
        x >= min && x == trunc(x)
}

assert_count <- function(x, min = 0L, null_ok = TRUE) {
    nm <- deparse(substitute(x))
    if (is.null(x)) {
        if (null_ok) return(x)

        stop(sprintf(
            "`%s` should be a single integer greater than `%s`, not `NULL`.",
            nm, min
        ))
    }

    if (!is_count(x, min)) {
        stop(sprintf(
            "`%s` should be a single integer greater than `%s` but is `%s`.",
            nm, min, x
        ))
    }

    as.integer(x)
}

is_flag <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
}

assert_flag <- function(x) {
    nm <- deparse(substitute(x))
    if (!is_flag(x)) {
        stop(sprintf(
            "`%s` should be either `TRUE` or `FALSE` but is `%s`.",
            nm, x
        ))
    }
    TRUE
}

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

check_packages <- function(pkgs) {
    has <- vapply(pkgs, requireNamespace, NA, quietly = TRUE)

    if (!all(has)) {
        miss <- pkgs[!has]
        stop(sprintf(
            "The %s %s needed for this app to run.",
            str_join(sQuote(miss), sep = ", "),
            ngettext(length(miss), "package is", "packages are")
        ))
    }
}
