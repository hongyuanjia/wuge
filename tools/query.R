# convert to GB18030 encoding
iconv_gb <- function(x) {
    chars <- strsplit(x, "")[[1L]]
    raws <- lapply(chars, function(char) {
        char <- as.character(charToRaw(iconv(char, "UTF-8", "GB18030")))
        paste0("%", toupper(char), collapse = "")
    })
    paste0(unlist(raws), collapse = "")
}

normalize_text <- function(nodes) {
    txt <- strsplit(xml2::xml_text(nodes), "\r\n", fixed = TRUE)
    lapply(txt, function(txt) trimws(txt[!grepl("^\\s*$", txt)], whitespace = "[ \t\r\n　]"))
}

remove_space <- function(x) gsub("[ \t\r\n　]", "", x)

ensure_period <- function(x) {
    no_period <- substring(x, nchar(x), nchar(x)) != "。"
    x[no_period] <- paste0(x[no_period], "。")
    x
}

clean_text <- function(x) ensure_period(remove_space(x))

get_req_node <- function(content, type) {
    xml2::xml_find_first(content,
        file.path(
            sprintf("//div[@class='chaxun_t' and b[text()='%s']]", type),
            "following-sibling::div[@class='chaxun_b'][1]"
        )
    )
}

parse_brief <- function(brief) {
    text <- normalize_text(brief)[[1L]]

    # 简体姓名
    name <- xml2::xml_attr(
        xml2::xml_find_all(
            xml2::xml_find_first(brief, "//tr[td[text()='　姓名：']]"),
            "./td/a[@title]"
        ),
        "title"
    )
    # 拼音
    pinyin <- normalize_text(xml2::xml_find_first(brief, "//tr[td[text()='　拼音：']]"))[[1L]][-1L]
    # 笔画
    stroke <- normalize_text(xml2::xml_find_first(brief, "//tr[td[text()='　笔画：']]"))[[1L]][-1L]
    stroke <- as.integer(gsub("画", "", stroke))
    # 五行
    wuxing <- normalize_text(xml2::xml_find_first(brief, "//tr[td[text()='　五行：']]"))[[1L]][-1L]

    dt_name <- data.table::data.table(name = name, pinyin = pinyin, stroke = stroke, wuxing = wuxing)

    # 五格吉凶
    jixiong <- normalize_text(xml2::xml_find_first(
        brief, "//td[starts-with(text(), '\r\n　天格')]"
    ))[[1L]]
    jixiong <- data.table::transpose(strsplit(jixiong, "[ ]?→ "))
    num_wuxing <- data.table::transpose(reg_match(jixiong[[2L]], "(\\d+)\\((.)\\)", 1:2))[-1L]
    dt_wuge <- data.table::data.table(
        wuge = jixiong[[1L]],
        number = as.integer(num_wuxing[[1L]]),
        wuxing = num_wuxing[[2L]],
        jixiong = jixiong[[3L]]
    )

    # 五格总分及基础运
    node_base_desc <- xml2::xml_find_first(brief,
        "//font[@class='blue' and text()='从五格互动看基础运：']"
    )
    score_wuge <- xml2::xml_text(xml2::xml_find_first(
        node_base_desc, "./following-sibling::font"
    ))
    score_wuge <- as.integer(reg_match(score_wuge, "\\d+"))
    desc <- reg_match(text[length(text)], "(?<=\\d分\\]).+")

    list(
        name = dt_name,
        wuge = list(
            score = score_wuge,
            brief = desc,
            detail = dt_wuge
        )
    )
}

parse_para_block <- function(node, prefix, detail) {
    brief <- as.list(xml2::xml_text(xml2::xml_find_all(
        node, "./font[@class='red']"
    )))
    ind_score <- length(brief) - 1L
    score <- as.double(reg_match(brief[[ind_score]], "\\d+"))
    jixiong <- brief[[length(brief)]]

    if (length(prefix) == 1L) {
        xpath_prefix <- sprintf("starts-with(text(), '%s')", prefix)
    } else {
        xpath_prefix <- sprintf("starts-with(text(), '%s') and contains(text(), '%s')",
            prefix[1], prefix[2]
        )
    }

    summary <- xml2::xml_text(
        xml2::xml_find_all(node,
            file.path(
                sprintf("./font[@class='blue' and %s]", xpath_prefix),
                "following-sibling::text()[following-sibling::font[@class='red' and contains(text(), '分')]]"
            )
        )
    )
    summary <- remove_space(summary)

    definition <- ensure_period(summary[1L])
    summary <- ensure_period(substring(summary[2L], 1L, nchar(summary[2L]) - 1L))

    num <- length(detail)
    details <- vector("list", num)
    for (i in seq_along(detail)) {
        if (i == num) {
            anchor_next <- ""
        } else {
            anchor_next <- sprintf("[following-sibling::b[text()='%s']]", detail[i + 1L])
        }

        details[[i]] <- clean_text(unlist(normalize_text(
            xml2::xml_find_all(node,
                sprintf("./b[text()='%s']/following-sibling::text()%s", detail[i], anchor_next)
            )
        )))
    }
    if (num == 1L) details <- details[[1L]]

    res <- list(definition = definition, score = score, jixiong = jixiong, brief = summary, detail = details)

    if (length(brief) == 3L) {
        res$wuxing <- brief[[1L]]
    }

    res
}

parse_shuli <- function(shuli) {
    name_wuge <- c("总格", "天格", "人格", "地格", "外格")
    len_wuge <- length(name_wuge)
    res <- vector("list", len_wuge)
    # have to put all extract wuge part into a new separate part
    doc <- xml2::read_html("<div class='chaxun_t'/>")
    div <- xml2::xml_find_first(doc, "//div")

    for (i in seq_len(len_wuge)) {
        xp_start <- sprintf("./font[@class='blue' and starts-with(text(), '%s')]", name_wuge[i])
        xp_cont <- sprintf("following-sibling::*")
        xp_text <- sprintf("following-sibling::text()")

        if (i == len_wuge) {
            xp_end <- ""
        } else {
            xp_end <- sprintf("[following::font[@class='blue' and starts-with(text(), '%s')]]", name_wuge[i + 1L])
        }

        xp <- sprintf(
            "%s | %s/%s%s |%s/%s%s",
            xp_start,
            xp_start, xp_cont, xp_end,
            xp_start, xp_text, xp_end
        )

        nodes <- xml2::xml_find_all(shuli, xp)
        xml2::xml_remove(xml2::xml_children(div))
        for (child in nodes) xml2::xml_add_child(div, child)

        block <- parse_para_block(div, c(name_wuge[i], "的解析："), c("详细解释：", "数理暗示："))

        res[[i]]$definition <- block$definition
        res[[i]]$score <- block$score
        res[[i]]$jixiong <- block$jixiong

        res[[i]] <- c(res[[i]], as.list(reg_match(block$brief, "（(?<brief>.+)）(?<short>.+)", 1:2)[-1L]))

        res[[i]]$description <- block$detail[[1L]]
        res[[i]]$fortune <- block$detail[[2L]]
    }

    names(res) <- c("zong", "tian", "ren", "di", "wai")
    res
}

parse_sancai <- function(sancai) {
    res <- parse_para_block(sancai, "三才配置：", "详细解释：")
    res$score <- as.integer(res$score)
    res$definition <- substring(res$definition, 2L)
    res$detail <- res$detail[-1L]
    res[c("definition", "score", "wuxing", "jixiong", "brief", "detail")]
}

parse_summary <- function(summary) {
    score <- as.double(xml2::xml_text(
        xml2::xml_find_first(summary, "./font[contains(text(), '分')]/b")
    ))

    suggestion <- xml2::xml_text(xml2::xml_find_first(summary, "./font[@class='red']"))

    list(score = score, suggestion = suggestion)
}

compose_query_params <- function(xing, ming, sex, birthday) {
    params <- list()

    # 0 for the Gregorian calendar，1 for the tridational Chinese calendar
    params$data_type <- "0"

    # validate input
    if (!is.character(xing) || length(xing) != 1L || is.na(xing)) {
        stop("Input 'xing' should be a non-NA string.")
    }
    if (!is.character(ming) || length(ming) != 1L || is.na(ming)) {
        stop("Input 'xing' should be a non-NA string.")
    }
    params$xing <- iconv_gb(paste0(xing, collapse = ""))
    params$ming <- iconv_gb(paste0(ming, collapse = ""))

    params$sex <- as.character(as.integer(match.arg(sex, c("男", "女")) == "男"))

    if (length(birthday) != 1L) {
        stop("Input 'birthday' should be a length-one string, Date or POSIXt.")
    }
    if (is.character(birthday)) {
        if (!is.na(dt <- as.POSIXct(birthday))) {
            birthday <- dt
        } else if (!is.na(dt <- as.Date(birthday))) {
            birthday <- dt
        } else {
            stop("When input 'birthday' is a string, it should be a valid format for Date or Datetime")
        }
    }
    if (inherits(birthday, "Date")) {
        s <- strsplit(format(birthday, "%Y %m %d"), " ", fixed = TRUE)[[1L]]
        params$year   <- s[1]
        params$month  <- s[2]
        params$day    <- s[3]
        params$hour   <- "0"
        params$minute <- "0"
    } else if (inherits(birthday, "POSIXt")) {
        s <- strsplit(format(birthday, "%Y %m %d %H %M"), " ", fixed = TRUE)[[1L]]
        params$year   <- s[1]
        params$month  <- s[2]
        params$day    <- s[3]
        params$hour   <- s[4]
        params$minute <- s[5]
    } else {
        stop("Input 'birthday' should be a length-one string, Date or POSIXt.")
    }

    # ignore 喜用五行
    params$wxxy <- "0"

    params$act <- "submit"
    params$isbz <- "1"

    paste0(names(params), "=", unlist(params, FALSE, FALSE), collapse = "&")
}

query_name <- function(xing, ming, sex, birthday) {
    fields <- charToRaw(compose_query_params(xing, ming, sex, birthday))

    handle <- curl::new_handle()
    curl::handle_setopt(handle,
        post = TRUE, postfieldsize = length(fields), postfields = fields
    )
    curl::handle_setheaders(handle, `Content-Type` = "application/x-www-form-urlencoded")

    url_request <- "http://life.httpcn.com/xingming.asp"
    res <- curl::curl_fetch_memory(url_request, handle)
    content <- iconv(rawToChar(res$content), from = "GB18030", to = "UTF-8")
    content <- xml2::read_html(content)

    brief <- parse_brief(get_req_node(content, "姓名五格排盘及互动关系"))
    shuli <- parse_shuli(get_req_node(content, "五格数理剖析"))
    sancai <- parse_sancai(get_req_node(content, "三才剖析"))
    summary <- parse_summary(get_req_node(content, "姓名评分及建议"))

    res <- list()
    res$suggestion <- summary$suggestion

    res$score <- c(
        total = summary$score,
        wuge = brief$wuge$score,
        sancai = sancai$score
    )

    res$name <- brief$name

    res$wuge <- brief$wuge
    res$wuge$detail <- cbind(
        res$wuge$detail,
        data.table::rbindlist(lapply(
            shuli[c("tian", "ren", "di", "wai", "zong")],
            function(sl) {
                mult <- lengths(sl) > 1L
                sl[mult] <- lapply(sl[mult], list)
                data.table::as.data.table(sl)
            }
        )
    ))

    res$sancai <- sancai

    res
}

reg_match <- function(x, pattern, n = NULL, flat = TRUE) {
    x <- as.character(x)

    match <- gregexpr(pattern, x, perl = TRUE)

    # number of groups
    ngrp <- length(attr(match[[1L]], "capture.names"))

    # stop if invalid capture group is specified
    if (!is.null(n) && any(invld_n <- ngrp < n)) {
        stop(sprintf(
            paste0(
                if (ngrp) {
                    "There are only %s capture groups in 'pattern'."
                } else {
                    "There are %s capture group in 'pattern'."
                },
                "Invalid 'n' found: [%s]."
            ),
            ngrp, paste0(n[invld_n], collapse = ", ")
        ))
    }

    # whether nothing is matched
    nomatch <- vapply(match, function(m) length(m) == 1L && m == -1L, logical(1L))

    # init results
    res <- replicate(length(x), list(), simplify = FALSE)

    if (any(nomatch)) {
        res[nomatch] <- list(NULL)
    }

    if (any(!nomatch)) {
        res[!nomatch] <- mapply(
            text = x[!nomatch], match = match[!nomatch], SIMPLIFY = FALSE,
            function(text, match) {
                whole <- substring(
                    text, match, match + attr(match, "match.length") - 1L
                )

                # directly return the whole match if n is NULL or no capture
                # groups specified
                if (is.null(n) || is.null(attr(match, "capture.start"))) return(whole)

                gstart <- attr(match, "capture.start")
                glength <- attr(match, "capture.length")
                gend <- gstart + glength - 1L

                groups <- substring(text, gstart, gend)
                dim(groups) <- dim(gstart)
                res <- cbind(whole, groups)
                colnames(res) <- c(".match", attr(match, "capture.name"))
                res
            }
        )
    }

    # only return whole match and specified groups
    if (!is.null(n)) res <- lapply(res, function(mat) mat[, c(1L, n + 1L), drop = FALSE])

    # only flat when every input has at least a match
    if (flat && !any(nomatch)) {
        # if all are a single row matrix, change to a character vector
        if (ngrp && all(vapply(res, nrow, integer(1)) == 1L)) {
            res <- lapply(res, function(x) {
                chr <- as.character(x)
                names(chr) <- colnames(x)
                chr
            })
        }

        # if input is a string, directly returns the results
        if (length(x) == 1L) res <- res[[1L]]
    }

    return(res)
}

query_char <- function(char) {
    if (!is.character(char) || length(char) != 1L || is.na(char)) {
        stop("Input 'char' should be a non-NA string.")
    }
    if (nchar(char) != 1L) {
        stop("Input 'char' should contain only one character.")
    }

    fields <- charToRaw(sprintf("Tid=1&wd=%s", curl::curl_escape(char)))

    handle <- curl::new_handle()
    curl::handle_setopt(handle,
        post = TRUE, postfieldsize = length(fields), postfields = fields
    )
    curl::handle_setheaders(handle, `Content-Type` = "application/x-www-form-urlencoded")

    url_request <- "https://tool.httpcn.com/KangXi/So.asp"

    res <- curl::curl_fetch_memory(url_request, handle)
    content <- xml2::read_html(res$content)

    desc <- xml2::xml_find_first(content,
        file.path(
            "./body/div[@class='content']/div[@class='content_m']/div[@id='div_a1']",
            "table/tr/td/p[@class='text15']"
        )
    )

    # remove script tags before extract description
    xml2::xml_remove(xml2::xml_find_all(desc, ".//script"))
    txt <- clean_text(xml2::xml_text(desc))
    if (grepl("繁体部首", txt)) {
        match <- reg_match(txt,
            paste0(
                "(?:繁体字：(?<trad>\\p{Han}+))?",
                "(?:异体字：\\p{Han}+)?",
                "拼音：(?<pinyin>\\P{Han}+)",
                ".+",
                "简体部首：(?<radical>.)",
                "部首笔画：(?<stroke_radical>\\d+)",
                "总笔画：(?<stroke>\\d+)",
                "繁体部首：(?<trad_radical>.)",
                "部首笔画：(?<trad_stroke_radical>\\d+)",
                "总笔画：(?<trad_stroke>\\d+)",
                "康熙字典笔画\\((?:.:(?<stroke_kangxi>\\d+))?.*\\)"
            ),
            1:9
        )[-1L]
        grepl("\\p{Han}", "緒", perl = TRUE)
        res <- c(list(char = char), as.list(match))
        if (!nzchar(res$trad)) res$trad <- res$char
    } else if (grepl("繁体字", txt)) {
        match <- reg_match(txt,
            paste0(
                "繁体字：(?<trad>\\p{Han}+)",
                "(?:异体字：\\p{Han}+)?",
                "拼音：(?<pinyin>\\P{Han}+)",
                ".*",
                "部首：(?<radical>.)",
                "部首笔画：(?<stroke_radical>\\d+)",
                "总笔画：(?<stroke>\\d+)",
                "康熙字典笔画\\((?:.:(?<stroke_kangxi>\\d+))?.*\\)"
            ),
            1:6
        )[-1L]
        res <- list()
        res$char <- char
        res$pinyin <- match[["pinyin"]]
        res$radical <- match[["radical"]]
        res$stroke <- match[["stroke"]]
        res$stroke_radical <- match[["stroke_radical"]]

        res$trad <- match[["trad"]]
        res$trad_radical <- res$radical
        res$trad_stroke <- res$stroke
        res$trad_stroke_radical <- res$stroke_radical

        res$stroke_kangxi <- match[["stroke_kangxi"]]
    } else if (grepl("简体字", txt)) {
        match <- reg_match(txt,
            paste0(
                "简体字：(?<char>.)",
                "(?:异体字：\\p{Han}+)?",
                "拼音：(?<pinyin>\\P{Han}+)",
                ".+",
                "部首：(?<radical>.)",
                "部首笔画：(?<stroke_radical>\\d+)",
                "总笔画：(?<stroke>\\d+)",
                "康熙字典笔画\\((?:.:(?<stroke_kangxi>\\d+))?.*\\)"
            ),
            1:6
        )[-1L]
        res <- list()
        res$char <- match[["char"]]
        res$pinyin <- match[["pinyin"]]
        res$radical <- NA_character_
        res$stroke <- NA_integer_
        res$stroke_radical <- NA_integer_

        res$trad <- char
        res$trad_radical <- match[["radical"]]
        res$trad_stroke <- match[["stroke"]]
        res$trad_stroke_radical <- match[["stroke_radical"]]

        res$stroke_kangxi <- match[["stroke_kangxi"]]
    } else if (grepl("部首：", txt)) {
        match <- reg_match(txt,
            paste0(
                "拼音：(?<pinyin>\\P{Han}+)",
                ".*",
                "部首：(?<radical>.)",
                "部首笔画：(?<stroke_radical>\\d+)",
                "总笔画：(?<stroke>\\d+)",
                "康熙字典笔画\\((?:.:(?<stroke_kangxi>\\d+))?.*\\)"
            ),
            1:5
        )[-1L]
        res <- list()
        res$char <- char
        res$pinyin <- match[["pinyin"]]
        res$radical <- match[["radical"]]
        res$stroke <- match[["stroke"]]
        res$stroke_radical <- match[["stroke_radical"]]

        res$trad <- char
        res$trad_radical <- res$radical
        res$trad_stroke <- res$stroke
        res$trad_stroke_radical <- res$stroke_radical

        res$stroke_kangxi <- match[["stroke_kangxi"]]
    } else {
        warning(sprintf("Internal error: failed to parse query results for character '%s'. Skip", char))
        res <- list(char = char, pinyin = NA_character_, radical = NA_character_,
            stroke = NA_integer_, stroke_radical = NA_integer_, wuxing = NA_character_,
            trad = NA_character_, trad_radical = NA_character_, trad_stroke = NA_integer_,
            trad_stroke_radical = NA_integer_, stroke_kangxi = NA_integer_
        )
        return(res)
    }

    if (!nzchar(res$stroke_kangxi)) {
        kangxi <- xml2::xml_text(xml2::xml_find_first(content,
            file.path(
                "./body/div[@class='content']/div[@class='content_m']/div[@id='div_a1']",
                "div[@class='content16']/strong/text()"
            )
        ))
        res$stroke_kangxi <- unname(reg_match(kangxi, "(?:康熙笔画：(?<stroke_kangxi>\\d+))", 1L)[2L])
    }

    res$stroke <- as.integer(res$stroke)
    res$stroke_radical <- as.integer(res$stroke_radical)
    res$trad_stroke_radical <- as.integer(res$trad_stroke_radical)
    res$stroke_kangxi <- as.integer(res$stroke_kangxi)

    wuxing <- xml2::xml_find_first(content,
        file.path(
            "./body/div[@class='content']/div[@class='content_m']/div[@id='div_a1']",
            "div[@class='text16' and span[contains(text(), '民俗参考')]]"
        )
    )
    txt <- clean_text(xml2::xml_text(wuxing))
    if (is.na(txt)) {
        wuxing <- NA_character_
    } else {
        wuxing <- reg_match(txt, "汉字五行：(?<wuxing>\\p{Han})", 1)[-1L]
    }
    if (!length(wuxing)) wuxing <- NA_character_

    res <- c(res, list(wuxing = unname(wuxing)))

    res[c("char", "pinyin", "radical", "stroke", "stroke_radical", "wuxing",
        "trad", "trad_radical", "trad_stroke", "trad_stroke_radical", "stroke_kangxi")]
}

query_all_common_char <- function(force = FALSE) {
    fpath <- here::here("tools/data/query/char.csv")
    if (!(fexist <- file.exists(fpath))) force <- TRUE

    if (fexist && !force) return(data.table::fread(fpath))

    char_common <- data.table::fread(system.file("extdata/char_common.csv", package = "wuge"))
    # find the corresponding kangxi stroke for every common character
    cli::cli_progress_bar(
        total = nrow(char_common), clear = FALSE,
        format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
    )
    res <- vector("list", nrow(char_common))
    # Kangxi strokes to query
    for (i in seq_along(res)) {
        cli::cli_progress_update(1L)
        res[[i]] <- tryCatch(
            query_char(char_common$character[[i]]),
            error = function(e) {
                cli::cli_alert_warning(conditionMessage(e))
                list(stroke_kangxi = NA_integer_)
            }
        )
    }

    data.table::set(char_common, NULL, "stroke", vapply(res, .subset2, integer(1L), "stroke_kangxi"))

    dt_char <- char_common[!is.na(stroke)][J(seq_len(81)), on = "stroke", .SD, .SDcols = c("character", "stroke"), mult = "first"]
    data.table::setnames(dt_char, c("xing", "total_stroke"))
    data.table::setcolorder(dt_char, "total_stroke")

    # Use a character that is not supported by the system. It will be treated as
    # zero stroke
    dt_char[!is.na(xing), `:=`(xing_stroke = total_stroke, ming1 = "齗", ming1_stroke = 0L)]
    data.table::set(dt_char, NULL, c("ming2", "ming2_stroke"), list(NA_character_, NA_integer_))

    # one character in ming
    have <- dt_char[!is.na(xing_stroke), total_stroke]
    miss <- dt_char[is.na(xing_stroke), total_stroke]
    comb1 <- data.table::CJ(xing_stroke = have, ming1_stroke = have)
    data.table::set(comb1, NULL, "total_stroke", comb1$xing_stroke + comb1$ming1_stroke)
    comb1 <- comb1[J(miss), on = "total_stroke", mult = "first"][!is.na(xing_stroke)]

    # two character in ming
    have <- unique(c(have, comb1$total_stroke))
    miss <- setdiff(seq_len(81), have)
    comb2 <- data.table::CJ(xing_stroke = have, ming1_stroke = have)
    data.table::set(comb2, NULL, "total_stroke", comb2$xing_stroke + comb2$ming1_stroke)
    comb2 <- comb2[J(miss), on = "total_stroke", mult = "first"][!is.na(xing_stroke)]
    comb2[comb1, on = c("ming1_stroke" = "total_stroke"), `:=`(ming1_stroke = i.xing_stroke, ming2_stroke = i.ming1_stroke)]

    # combine all
    comb <- data.table::rbindlist(list(comb1, comb2), fill = TRUE)
    comb[dt_char[!is.na(xing)], on = c("xing_stroke"), `:=`(xing = i.xing)]
    comb[dt_char[!is.na(xing)], on = c("ming1_stroke" = "xing_stroke"), `:=`(ming1 = i.xing)]
    comb[dt_char[!is.na(xing)], on = c("ming2_stroke" = "xing_stroke"), `:=`(ming2 = i.xing)]
    comb[is.na(ming2_stroke), `:=`(ming2_stroke = 0L)]

    char <- data.table::rbindlist(list(dt_char[!is.na(xing)], comb), use.names = TRUE)
    data.table::setcolorder(char, c("xing", "ming1", "ming2", "total_stroke", "xing_stroke", "ming1_stroke", "ming2_stroke"))

    # it is possible the characters with 5 strokes are incorrect
    # here use '巨' which is guaranteed to be correct
    char[xing_stroke == 5L, xing := "巨"]
    char[ming1_stroke == 5L, ming1 := "巨"]
    char[ming2_stroke == 5L, ming2 := "巨"]

    if (!dir.exists(dirname(fpath))) dir.create(dirname(fpath))
    data.table::fwrite(char, fpath)
    char
}

query_all_shuli <- function(force = FALSE) {
    fpath <- here::here("tools/data/query/shuli.csv")
    if (!(fexist <- file.exists(fpath))) force <- TRUE
    if (fexist && !force) return(data.table::fread(fpath))

    if (!file.exists(here::here("tools/data/query/char.csv"))) {
        query_all_common_char()
    }
    char <- data.table::fread(here::here("tools/data/query/char.csv"))
    char[, by = "total_stroke", c("ming", "ming_stroke") := {
        strokes <- c(ming1_stroke, ming2_stroke[!is.na(ming2_stroke) & ming2_stroke != 0L])
        list(
            ming = paste0(ming1, ming2[!is.na(ming2)]),
            ming_stroke = list(strokes[!is.na(strokes)])
        )
    }]

    cli::cli_progress_bar(
        total = nrow(char), clear = FALSE,
        format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
    )
    res <- vector("list", nrow(char))
    for (i in seq_along(res)) {
        cli::cli_progress_output("Query Shuli={.val {i}}...")
        cli::cli_progress_update(1L)
        res[[i]] <- tryCatch(
            query_name(char$xing[i], char$ming[i], "男", Sys.Date()),
            error = function(e) {
                cli::cli_progress_message(conditionMessage(e))
                list()
            }
        )
    }

    shuli <- data.table::rbindlist(
        lapply(res, function(r) r$wuge$detail[5L, -c("definition", "wuge")])
    )

    data.table::set(shuli, NULL, c("description", "fortune"),
        list(
            vapply(shuli$description, paste0, "", collapse = "||"),
            vapply(shuli$fortune, paste0, "", collapse = "||")
        )
    )

    if (!dir.exists(dirname(fpath))) dir.create(dirname(fpath))
    data.table::fwrite(shuli, fpath)
    shuli
}

query_all_sancai <- function(force = FALSE) {
    fpath <- here::here("tools/data/query/sancai.csv")
    if (!(fexist <- file.exists(fpath))) force <- TRUE
    if (fexist && !force) return(data.table::fread(fpath))

    if (!file.exists(here::here("tools/data/query/char.csv"))) {
        query_all_common_char()
    }
    char <- data.table::fread(here::here("tools/data/query/char.csv"))
    char <- char[ming1_stroke == 0L]
    strokes <- char$xing_stroke

    wuxing <- c("金", "木", "水", "火", "土")
    full_sancai <- do.call(paste0, data.table::CJ(wuxing, wuxing, wuxing))

    # one char in xing & one char in ming
    get_wuxing <- function(stroke) {
        mod <- stroke %% 10L
        data.table::fcase(
            mod %in% c(1L, 2L), "木",
            mod %in% c(3L, 4L), "火",
            mod %in% c(5L, 6L), "土",
            mod %in% c(7L, 8L), "金",
            mod %in% c(9L, 0L), "水"
        )
    }
    get_tian <- function(xing_stroke, ming_stroke) {
        get_wuxing(if (length(xing_stroke) == 1L) xing_stroke + 1L else sum(xing_stroke))
    }
    get_ren <- function(xing_stroke, ming_stroke) {
        get_wuxing(xing_stroke[length(xing_stroke)] + ming_stroke[1L])
    }
    get_di <- function(xing_stroke, ming_stroke) {
        get_wuxing(if (length(ming_stroke) == 1L) ming_stroke[1L] + 1L else sum(ming_stroke))
    }
    get_sancai <- function(xing_stroke, ming_stroke) {
        paste0(
            get_tian(xing_stroke, ming_stroke),
            get_ren(xing_stroke, ming_stroke),
            get_di(xing_stroke, ming_stroke)
        )
    }

    comb <- data.table::CJ(xing_stroke = strokes, ming1_stroke = strokes, ming2_stroke = strokes)
    comb[, by = seq_len(nrow(comb2)), `:=`(ming_stroke = list(c(ming1_stroke, ming2_stroke)))]

    ind <- c()
    found <- c()
    target <- full_sancai
    for (i in seq_len(nrow(comb))) {
        if (!length(target)) break
        cur <- get_sancai(comb$xing_stroke[[i]], comb$ming_stroke[[i]])
        if (cur %in% target) {
            target <- setdiff(target, cur)
            found <- c(found, cur)
            ind <- c(ind, i)
        }
    }
    comb <- comb[ind]
    data.table::set(comb, NULL, "ming_stroke", NULL)
    data.table::set(comb, NULL, "sancai", found)
    comb[char, on = "xing_stroke", xing := i.xing]
    comb[char, on = c("ming1_stroke" = "xing_stroke"), ming1 := i.xing]
    comb[char, on = c("ming2_stroke" = "xing_stroke"), ming2 := i.xing]
    comb <- comb[, list(sancai, xing, ming = paste0(ming1, ming2))]

    cli::cli_progress_bar(
        total = nrow(comb), clear = FALSE,
        format = "[{cli::pb_current}/{cli::pb_total}] | {cli::pb_percent} {cli::pb_bar} [Elapsed: {cli::pb_elapsed}]"
    )
    res <- vector("list", nrow(comb))
    for (i in seq_along(res)) {
        cli::cli_progress_output("Query Sancai={.val {comb$sancai[[i]]}}...")
        cli::cli_progress_update(1L)
        res[[i]] <- tryCatch(
            query_name(comb$xing[i], comb$ming[i], "男", Sys.Date()),
            error = function(e) {
                cli::cli_progress_message(conditionMessage(e))
                list()
            }
        )
    }

    sancai <- data.table::rbindlist(
        lapply(res, function(r) {
            out <- r$sancai
            out$definition <- NULL
            out$detail <- paste0(out$detail, collapse = "||")
            out
        })
    )

    if (!dir.exists(dirname(fpath))) dir.create(dirname(fpath))
    data.table::fwrite(sancai, fpath)
    sancai
}
