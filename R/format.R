# see: https://github.com/r-lib/cli/issues/509
spacing <- function(x) gsub(" ", "\u00a0", x)

get_wuge_name <- function(x) {
    if (!"id" %in% names(x$ming)) {
        paste0(paste0(x$xing$character, collapse = ""), paste0(x$ming$character, collapse = ""))
    } else {
        paste0(paste0(x$xing$character, collapse = ""), paste0("\u3010", x$ming$stroke_wuge, "\u3011", collapse = ""))
    }
}

print_wuge_name <- function(x, name = NULL) {
    if ("id" %in% names(x$ming)) {
        x$ming <- data.table::data.table(
            index = x$ming$index, character = "*", stroke = "*",
            pinyin = "*", traditional = "*", stroke_wuge = x$ming$stroke_wuge
        )
    }

    char <- data.table::rbindlist(list(x$xing, x$ming), fill = TRUE)[, -1L]
    fmt <- lapply(data.table::transpose(char), format.default, justify = "right")
    fmt <- vapply(data.table::transpose(fmt), paste0, "", collapse = " ")
    names(fmt) <- names(char)

    cli::cli_rule("\u59d3\u540d", right = name)

    map <- c("character", "traditional", "pinyin", "stroke_wuge")
    names(map) <- c("\u59d3\u540d", "\u7e41\u4f53", "\u62fc\u97f3", "\u7b14\u753b")
    bul <- sprintf("%s\uff1a%s", names(map), spacing(fmt[map]))
    names(bul) <- rep("*", length(map))
    cli::cli_bullets(bul)

    invisible(x)
}

print_wuge_num <- function(x, name = NULL, box = TRUE) {
    ind <- 1:5
    names(ind) <- c("\u5929\u683c", "\u4eba\u683c", "\u5730\u683c", "\u5916\u683c", "\u603b\u683c")

    wuge <- x$shuli[, lapply(.SD, as.character), .SDcols = c("num", "wuxing", "jixiong")]
    fmt <- data.table::transpose(lapply(wuge, format.default, justify = "right"))
    names(fmt) <- names(ind)
    fmt <- vapply(fmt, paste0, "", collapse = " -> ")

    cli::cli_rule("\u4e94\u683c\u6570\u7406\u6392\u76d8", right = name)

    if (box) {
        cli::cat_boxx(spacing(sprintf(" %s \u5206 ", x$score$score_shuli)), padding = 0)
    } else {
        cli::cli_text(spacing(sprintf(" \u3010%s \u5206\u3011", x$score$score_shuli)))
    }

    bul <- sprintf("%s\uff1a%s", names(ind), spacing(fmt[ind]))
    names(bul) <- rep("*", length(ind))
    cli::cli_bullets(bul)

    invisible(x)
}

print_wuge_shuli <- function(x, name = NULL, box = TRUE) {
    ind <- 1:5
    names(ind) <- c("\u5929\u683c", "\u4eba\u683c", "\u5730\u683c", "\u5916\u683c", "\u603b\u683c")

    for (i in seq_along(ind)) {
        cli::cli_rule(
            "{names(ind)[i]} {.val {x$shuli$num[i]}} \u7684\u5206\u6790",
            right = name
        )

        if (box) {
            cli::cat_boxx(spacing(sprintf(" %s \u5206 ", x$shuli$score[i])), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" \u3010%s \u5206\u3011", x$shuli$score[i])))
        }

        cli::cli_bullets(c(
            "*" = "\u5173\u952e\u8bcd\uff1a{x$shuli$brief[i]}",
            "*" = "\u7b80\u8ff0\uff1a{x$shuli$short[i]}"
        ))

        cli::cli_bullets(c("*" = "\u6570\u7406\u6697\u793a\uff1a"))
        div <- cli::cli_div(theme = list(".bullets .bullet-*" = list("padding-left" = 2)))
        items <- strsplit(x$shuli$indication[i], "\n", fixed = TRUE)[[1L]]
        names(items) <- rep("*", length(items))
        cli::cli_bullets(items)
        cli::cli_end(div)

        cli::cli_bullets(c(
            "*" = "\u57fa\u7840\u8fd0\uff1a{x$shuli$foundation[i]}",
            "*" = "\u5bb6\u5ead\u8fd0\uff1a{x$shuli$family[i]}",
            "*" = "\u5065\u5eb7\u8fd0\uff1a{x$shuli$health[i]}",
            "*" = "\u4e8b\u4e1a\u8fd0\uff1a{x$shuli$future[i]}",
            "*" = "\u8d22\u5bcc\u8fd0\uff1a{x$shuli$fortune[i]}"
        ))

        cli::cli_bullets(c("*" = "\u8be6\u7ec6\u89e3\u91ca\uff1a"))
        div <- cli::cli_div(theme = list(".bullets .bullet-space" = list("padding-left" = 0)))
        items <- strsplit(x$shuli$description[i], "\n", fixed = TRUE)[[1L]]
        names(items) <- rep(" ", length(items))
        cli::cli_bullets(items)
        cli::cli_end(div)

        if (i < length(ind)) cli::cat_line()
    }

    invisible(x)
}

print_wuge_sancai <- function(x, name = NULL, box = TRUE) {
    cli::cli_rule("\u4e09\u624d\u5256\u6790", right = name)

    if (box) {
        cli::cat_boxx(
            spacing(sprintf(" \u5409\u51f6\uff1a<%s>\uff1b\u5f97\u5206\uff1a%s ", x$sancai$jixiong, x$sancai$score)),
            x$sancai$wuxing,
            padding = 0
        )
    } else {
        cli::cli_text(spacing(sprintf(" \u3010\u4e94\u884c\uff1a<%s>\uff1b\u5409\u51f6\uff1a<%s>\uff1b\u5f97\u5206\uff1a%s\u3011",
            x$sancai$wuxing, x$sancai$jixiong, x$sancai$score
        )))
    }

    cli::cli_rule("\u7b80\u8ff0", right = name)
    div <- cli::cli_div(theme = list(".bullets .bullet-space" = list("padding-left" = 2)))
    cli::cli_bullets(c(" " = x$sancai$brief, " " = x$sancai$description))
    cli::cli_end(div)

    cli::cat_line()

    cli::cli_rule("\u8be6\u7ec6\u89e3\u91ca", right = name)
    items <- gsub("^\\d+\u3001", "", strsplit(x$sancai$detail, "\n", fixed = TRUE)[[1L]])
    names(items) <- rep("*", length(items))
    cli::cli_bullets(items)
    if (!is.na(x$sancai$description_special)) {
        cli::cli_bullets(c("*" = "\u3010\u6ce8\u610f\u5065\u5eb7\uff01\u3011\uff1a{x$sancai$description_special}"))
    }

    invisible(x)
}

print_wuge_luck <- function(x, name = NULL, box = TRUE) {
    comp <- c("base", "success", "social")
    names(comp) <- c("\u57fa\u7840\u8fd0", "\u6210\u529f\u8fd0", "\u793e\u4ea4\u8fd0")

    map <- c("tian", "ren", "di", "wai", "zong")
    names(map) <- c("\u5929\u683c", "\u4eba\u683c", "\u5730\u683c", "\u5916\u683c",  "\u603b\u683c")

    for (i in seq_along(comp)) {
        cli::cli_rule(names(comp[i]), right = name)

        dt <- x$luck[[comp[[i]]]]
        if (box) {
            cli::cat_boxx(spacing(sprintf(" \u5409\u51f6\uff1a<%s> ", dt$jixiong)), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" \u3010\u5409\u51f6\uff1a<%s>\u3011", dt$jixiong)))
        }

        m <- map[map %in% names(dt)]
        s <- paste0(sprintf("%s\u3010%s\u3011", names(m), c(dt[[m[1L]]], dt[[m[2L]]])), collapse = "")

        cli::cli_bullets(c("*" = "\u4e94\u683c\uff1a{s}"))
        cli::cli_bullets(c("*" = "\u63cf\u8ff0\uff1a{dt$description}"))
        if (i < length(comp)) cli::cat_line()
    }

    if (!is.na(x$luck$health$description)) {
        cli::cli_rule("\u5065\u5eb7\u8fd0", right = name)

        if (box) {
            cli::cat_boxx(spacing(sprintf(" \u5409\u51f6\uff1a<\u51f6> ")), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" \u3010\u5409\u51f6\uff1a<\u51f6>\u3011")))
        }

        cli::cli_bullets(c("*" = "\u4e94\u683c\uff1a\u5929\u683c\u3010{x$luck$health$tian}\u3011\u4eba\u683c\u3010{x$luck$health$di}\u3011"))
        cli::cli_bullets(c("*" = "\u63cf\u8ff0\uff1a{x$luck$health$description}"))
    }

    invisible(x)
}

print_wuge_single <- function(x, box = TRUE) {
    name <- get_wuge_name(x)
    div <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
    if ("id" %in% names(x$score) && (!length(x$score$id) || is.na(x$score$id))) {
        cli::cli_rule("\u59d3\u540d", right = "{paste0(x$xing$character, collapse = \'\')} *")
        cli::cli_text(" {spacing(\' \')} <{.emph \u65e0\u6ee1\u8db3\u8981\u6c42\u7684\u7b14\u753b\u7ec4\u5408\u3002}>")
        return(invisible(x))
    }

    print_wuge_name(x, name)
    cli::cli_end(div)
    cat("\n")
    print_wuge_num(x, name, box = box)
    cat("\n")
    print_wuge_shuli(x, name, box = box)
    cat("\n")
    print_wuge_sancai(x, name, box = box)
    cat("\n")
    print_wuge_luck(x, name, box = box)

    return(invisible(x))
}

format_wuge_single <- function(x, width = NULL, strip = TRUE) {
    op <- options("cli.width" = width)
    on.exit(options(op), add = TRUE)
    fmt <- cli::cli_format_method(print_wuge_single(x, box = FALSE))
    if (strip) fmt <- cli::ansi_strip(fmt)
    if (!is.null(width)) fmt <- strtrunk(fmt, width)
    fmt
}

strtrunk <- function(x, width = cli::console_width()) {
    nc <- cli::ansi_nchar(x, "width")
    if (!any(long <- nc > width)) return(x)

    out <- vector("list", sum(long))
    nc <- nc[long]
    txt <- x[long]
    ind <- seq_along(txt)

    # detect leading characters
    pre <- unlist(regmatches(txt, gregexpr("^\\P{Han}+", txt, perl = TRUE)))
    pre <- cli::ansi_strip(pre)
    # get the first pos of bullet character
    not_spc <- vapply(gregexpr("\\*|\u2022", pre, perl = TRUE), .subset2, 1L, 1L)
    # replace the prefix with spaces
    pre[not_spc > 0L] <- strrep(" ", not_spc[not_spc > 0L] + 1L)
    # for only-space prefix, do not keep it
    pre[not_spc == -1L] <- ""

    first <- TRUE

    while (length(ind)) {
        line <- if (!first) {
            paste0(
                pre[ind],
                cli::ansi_strtrim(txt[ind], width - cli::ansi_nchar(pre[ind]), ""),
                "\n"
            )
        } else {
            paste0(cli::ansi_strtrim(txt[ind], width, ""), "\n")
        }

        first <- FALSE

        for (i in seq_along(ind)) {
            out[[ind[i]]] <- c(out[[ind[i]]], line[[i]])
        }
        txt[ind] <- cli::ansi_substring(txt[ind], cli::ansi_nchar(line))
        nc[ind] <- cli::ansi_nchar(txt[ind], "width")
        if (any(complete <- nc[ind] == 0L)) {
            ind <- ind[!complete]
        }
    }
    x[long] <- gsub("\n$", "", vapply(out, paste0, "", collapse = ""))
    x
}

subset_wuge <- function(x, i, j = NULL) {
    # NOTE: to make CRAN checks happ
    J <- NULL
    if (is.null(j)) {
        list(
            xing = x$xing[J(i), on = "index"],
            ming = x$ming[J(i), on = "index"],
            score = x$score[J(i), on = "index"],
            sancai = x$sancai[J(i), on = "index"],
            shuli = x$shuli[J(i), on = "index"],
            luck = lapply(x$luck, function(dt) dt[J(i), on = "index"])
        )
    } else {
        list(
            xing = x$xing[J(i), on = "index"],
            ming = x$ming[J(i, j), on = c("index", "id")],
            score = x$score[J(i, j), on = c("index", "id")],
            sancai = x$sancai[J(i, j), on = c("index", "id")],
            shuli = x$shuli[J(i, j), on = c("index", "id")],
            luck = lapply(x$luck, function(dt) dt[J(i, j), on = c("index", "id")])
        )
    }
}

#' @export
format.WuGe <- function(x, ...) {
    fmt <- c()
    for (i in x$score$index) {
        fmt <- format_wuge_single(subset_wuge(x, i), ...)
        if (i < x$score$index[nrow(x$score)]) c(fmt, "\n")
    }
    fmt
}

#' @export
print.WuGe <- function(x, ...) {
    for (i in x$score$index) {
        print_wuge_single(subset_wuge(x, i))
        if (i < x$score$index[nrow(x$score)]) cat("\n")
    }
}

#' @export
format.WuGeName <- function(x, ...) {
    fmt <- c()

    # NOTE: to make CRAN checks happy
    J <- NULL
    index <- split(x$score[J(unique(x$xing$index)), on = "index",
        .SD, .SDcols = c("index", "id")], by = c("index", "id")
    )
    i <- 0L
    for (ind in index) {
        fmt <- format_wuge_single(subset_wuge(x, ind$index, ind$id), ...)
        i <- i + 1L
        if (i < length(index)) c(fmt, "\n")
    }
    fmt
}

#' @export
print.WuGeName <- function(x, ...) {
    # NOTE: to make CRAN checks happy
    J <- NULL
    index <- split(x$score[J(unique(x$xing$index)), on = "index",
        .SD, .SDcols = c("index", "id")], by = c("index", "id")
    )
    i <- 0L
    for (ind in index) {
        print_wuge_single(subset_wuge(x, ind$index, ind$id), ...)
        i <- i + 1L
        if (i < length(index)) cat("\n")
    }
}
