# see: https://github.com/r-lib/cli/issues/509
spacing <- function(x) gsub(" ", "\u00a0", x)

get_wuge_name <- function(x) {
    if (!"id" %in% names(x$ming)) {
        paste0(paste0(x$xing$character, collapse = ""), paste0(x$ming$character, collapse = ""))
    } else {
        paste0(paste0(x$xing$character, collapse = ""), paste0("【", x$ming$stroke_wuge, "】", collapse = ""))
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

    cli::cli_rule("姓名", right = name)

    map <- c("姓名" = "character", "繁体" = "traditional", "拼音" = "pinyin", "笔画" = "stroke_wuge")
    bul <- sprintf("%s：%s", names(map), spacing(fmt[map]))
    names(bul) <- rep("*", length(map))
    cli::cli_bullets(bul)

    invisible(x)
}

print_wuge_num <- function(x, name = NULL, box = TRUE) {
    ind <- c("天格" = 1L, "人格" = 2L, "地格" = 3L, "外格" = 4L, "总格" = 5L)

    wuge <- x$shuli[, lapply(.SD, as.character), .SDcols = c("num", "wuxing", "jixiong")]
    fmt <- data.table::transpose(lapply(wuge, format.default, justify = "right"))
    names(fmt) <- names(ind)
    fmt <- vapply(fmt, paste0, "", collapse = " -> ")

    cli::cli_rule("五格数理排盘", right = name)

    if (box) {
        cli::cat_boxx(spacing(sprintf(" %s 分 ", x$score$score_shuli)), padding = 0)
    } else {
        cli::cli_text(spacing(sprintf(" 【%s 分】", x$score$score_shuli)))
    }

    bul <- sprintf("%s：%s", names(ind), spacing(fmt[ind]))
    names(bul) <- rep("*", length(ind))
    cli::cli_bullets(bul)

    invisible(x)
}

print_wuge_shuli <- function(x, name = NULL, box = TRUE) {
    ind <- c("天格" = 1L, "人格" = 2L, "地格" = 3L, "外格" = 4L, "总格" = 5L)

    for (i in seq_along(ind)) {
        cli::cli_rule(
            "{names(ind)[i]} {.val {x$shuli$num[i]}} 的分析",
            right = name
        )

        if (box) {
            cli::cat_boxx(spacing(sprintf(" %s 分 ", x$shuli$score[i])), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" 【%s 分】", x$shuli$score[i])))
        }

        cli::cli_bullets(c(
            "*" = "关键词：{x$shuli$brief[i]}",
            "*" = "简述：{x$shuli$short[i]}"
        ))

        cli::cli_bullets(c("*" = "数理暗示："))
        div <- cli::cli_div(theme = list(".bullets .bullet-*" = list("padding-left" = 2)))
        items <- strsplit(x$shuli$indication[i], "\n", fixed = TRUE)[[1L]]
        names(items) <- rep("*", length(items))
        cli::cli_bullets(items)
        cli::cli_end(div)

        cli::cli_bullets(c(
            "*" = "基础运：{x$shuli$foundation[i]}",
            "*" = "家庭运：{x$shuli$family[i]}",
            "*" = "健康运：{x$shuli$health[i]}",
            "*" = "事业运：{x$shuli$future[i]}",
            "*" = "财富运：{x$shuli$fortune[i]}"
        ))

        cli::cli_bullets(c("*" = "详细解释："))
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
    cli::cli_rule("三才剖析", right = name)

    if (box) {
        cli::cat_boxx(
            spacing(sprintf(" 吉凶：<%s>；得分：%s ", x$sancai$jixiong, x$sancai$score)),
            x$sancai$wuxing,
            padding = 0
        )
    } else {
        cli::cli_text(spacing(sprintf(" 【五行：<%s>；吉凶：<%s>；得分：%s】",
            x$sancai$wuxing, x$sancai$jixiong, x$sancai$score
        )))
    }

    cli::cli_rule("简述", right = name)
    div <- cli::cli_div(theme = list(".bullets .bullet-space" = list("padding-left" = 2)))
    cli::cli_bullets(c(" " = x$sancai$brief, " " = x$sancai$description))
    cli::cli_end(div)

    cli::cat_line()

    cli::cli_rule("详细解释", right = name)
    items <- gsub("^\\d+、", "", strsplit(x$sancai$detail, "\n", fixed = TRUE)[[1L]])
    names(items) <- rep("*", length(items))
    cli::cli_bullets(items)
    if (!is.na(x$sancai$description_special)) {
        cli::cli_bullets(c("*" = "【注意健康！】：{x$sancai$description_special}"))
    }

    invisible(x)
}

print_wuge_luck <- function(x, name = NULL, box = TRUE) {
    comp <- c("基础运" = "base", "成功运" = "success", "社交运" = "social")
    map <- c("天格" = "tian", "人格" = "ren", "地格" = "di", "外格" = "wai", "总格" = "zong")

    for (i in seq_along(comp)) {
        cli::cli_rule(names(comp[i]), right = name)

        dt <- x$luck[[comp[[i]]]]
        if (box) {
            cli::cat_boxx(spacing(sprintf(" 吉凶：<%s> ", dt$jixiong)), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" 【吉凶：<%s>】", dt$jixiong)))
        }

        m <- map[map %in% names(dt)]
        s <- paste0(sprintf("%s【%s】", names(m), c(dt[[m[1L]]], dt[[m[2L]]])), collapse = "")

        cli::cli_bullets(c("*" = "五格：{s}"))
        cli::cli_bullets(c("*" = "描述：{dt$description}"))
        if (i < length(comp)) cli::cat_line()
    }

    if (!is.na(x$luck$health$description)) {
        cli::cli_rule("健康运", right = name)

        if (box) {
            cli::cat_boxx(spacing(sprintf(" 吉凶：<凶> ")), padding = 0)
        } else {
            cli::cli_text(spacing(sprintf(" 【吉凶：<凶>】")))
        }

        cli::cli_bullets(c("*" = "五格：天格【{x$luck$health$tian}】人格【{x$luck$health$di}】"))
        cli::cli_bullets(c("*" = "描述：{x$luck$health$description}"))
    }

    invisible(x)
}

print_wuge_single <- function(x, box = TRUE) {
    name <- get_wuge_name(x)
    div <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
    if ("id" %in% names(x$score) && (!length(x$score$id) || is.na(x$score$id))) {
        cli::cli_rule("姓名", right = "{paste0(x$xing$character, collapse = '')} *")
        cli::cli_text(" {spacing(' ')} <{.emph 无满足要求的笔画组合。}>")
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
