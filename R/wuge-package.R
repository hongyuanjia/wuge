#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
## usethis namespace: end
NULL

BBN_ENV <- new.env(parent = emptyenv())
BBN_ENV$conv <- NULL
BBN_ENV$char <- NULL
BBN_ENV$fullchar <- NULL
BBN_ENV$kangxi <- NULL
BBN_ENV$sancai <- NULL
BBN_ENV$special_sancai <- NULL
BBN_ENV$luck_base <- NULL
BBN_ENV$luck_success <- NULL
BBN_ENV$luck_social <- NULL
BBN_ENV$luck_health <- NULL

dict_conv <- function(force = FALSE) {
    if (is.null(BBN_ENV$conv)) force <- TRUE

    if (force) {
        path <- system.file("extdata/stconv.csv", package = "wuge")
        conv <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(conv, "simplified")
        BBN_ENV$conv <- conv
    }

    BBN_ENV$conv
}

dict_char <- function(force = FALSE) {
    if (is.null(BBN_ENV$char)) force <- TRUE

    if (force) {
        path <- system.file("extdata/char.csv", package = "wuge")
        char <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(char, "character")
        BBN_ENV$char <- char
    }

    BBN_ENV$char
}

dict_kangxi <- function(force = FALSE) {
    if (is.null(BBN_ENV$kangxi)) force <- TRUE

    if (force) {
        path <- system.file("extdata/kangxi.csv", package = "wuge")
        kangxi <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(kangxi, "character")
        BBN_ENV$kangxi <- kangxi
    }

    BBN_ENV$kangxi
}

dict_fullchar <- function(force = FALSE) {
    if (is.null(BBN_ENV$fullchar)) force <- TRUE

    if (force) {
        fullchar <- data.table::copy(dict_char())
        fullchar[dict_conv(),
            on = c("character" = "simplified"),
            traditional := substring(i.traditional, 1L, 1L)
        ]
        fullchar[is.na(traditional), traditional := character]
        fullchar[dict_kangxi(),
            on = c("traditional" = "character"),
            stroke_wuge := i.stroke
        ]
        fullchar[is.na(stroke_wuge), stroke_wuge := stroke]

        fixed <- data.table::data.table(
            character = c(
                "\u4e00", "\u4e8c", "\u4e09", "\u56db", "\u4e94",
                "\u516d", "\u4e03", "\u516b", "\u4e5d", "\u5341"
                ),
            stroke = 1:10
        )

        fullchar[fixed, on = "character", stroke_wuge := i.stroke]
        data.table::setindexv(fullchar, "character")
        data.table::setindexv(fullchar, "traditional")
        data.table::setindexv(fullchar, "stroke")
        data.table::setindexv(fullchar, "stroke_wuge")

        BBN_ENV$fullchar <- fullchar
    }

    BBN_ENV$fullchar
}

dict_sancai <- function(force = FALSE) {
    if (is.null(BBN_ENV$sancai)) force <- TRUE

    if (force) {
        path <- system.file("extdata/sancai.csv", package = "wuge")
        sancai <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(sancai, "wuxing")
        BBN_ENV$sancai <- sancai
    }

    BBN_ENV$sancai
}

dict_special_sancai <- function(force = FALSE) {
    if (is.null(BBN_ENV$special_sancai)) force <- TRUE

    if (force) {
        path <- system.file("extdata/special_sancai.csv", package = "wuge")
        special_sancai <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(special_sancai, "wuxing")
        BBN_ENV$special_sancai <- special_sancai
    }

    BBN_ENV$special_sancai
}

dict_shuli <- function(force = FALSE) {
    if (is.null(BBN_ENV$shuli)) force <- TRUE

    if (force) {
        path <- system.file("extdata/shuli.csv", package = "wuge")
        shuli <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(shuli, "num")
        BBN_ENV$shuli <- shuli
    }

    BBN_ENV$shuli
}

dict_luck_base <- function(force = FALSE) {
    if (is.null(BBN_ENV$luck_base)) force <- TRUE

    if (force) {
        path <- system.file("extdata/luck_base.csv", package = "wuge")
        luck_base <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(luck_base, c("ren", "di"))
        BBN_ENV$luck_base <- luck_base
    }

    BBN_ENV$luck_base
}

dict_luck_success <- function(force = FALSE) {
    if (is.null(BBN_ENV$luck_success)) force <- TRUE

    if (force) {
        path <- system.file("extdata/luck_success.csv", package = "wuge")
        luck_success <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(luck_success, c("ren", "tian"))
        BBN_ENV$luck_success <- luck_success
    }

    BBN_ENV$luck_success
}

dict_luck_social <- function(force = FALSE) {
    if (is.null(BBN_ENV$luck_social)) force <- TRUE

    if (force) {
        path <- system.file("extdata/luck_social.csv", package = "wuge")
        luck_social <- data.table::fread(path, encoding = "UTF-8")
        data.table::setindexv(luck_social, c("ren", "wai"))
        BBN_ENV$luck_social <- luck_social
    }

    BBN_ENV$luck_social
}

dict_luck_health <- function(force = FALSE) {
    if (is.null(BBN_ENV$luck_health)) force <- TRUE

    if (force) {
        path_sancai <- system.file("extdata/luck_health_sancai.csv", package = "wuge")
        path_desc <- system.file("extdata/luck_health_desc.csv", package = "wuge")
        luck_health <- data.table::fread(path_sancai, encoding = "UTF-8")
        desc <- data.table::fread(path_desc, encoding = "UTF-8")

        # NOTE: to make CRAN checks happy
        description <- i.description <- NULL
        luck_health[desc, on = "index", description := i.description]
        data.table::set(luck_health, NULL, "index", NULL)
        data.table::setindexv(luck_health, c("tian", "ren", "di"))
        BBN_ENV$luck_health <- luck_health
    }

    BBN_ENV$luck_health
}
