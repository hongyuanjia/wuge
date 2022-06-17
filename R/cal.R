get_wuge_char_data <- function(char, to_trad = TRUE) {
    match <- get_char_data(char)
    if (to_trad) {
        data.table::set(match, NULL, "traditional", sim2trad(match$character))
    } else {
        data.table::set(match, NULL, "traditional", match$character)
    }

    # NOTE: right join is much faster than join and modify in place using `:=`
    match <- dict_kangxi()[, .SD, .SDcols = c("character", "stroke")][
        match, on = c("character" = "traditional")]
    data.table::setnames(match,
        c("i.stroke", "stroke", "character", "i.character"),
        c("stroke", "stroke_wuge", "traditional", "character")
    )

    # no need to include radical
    data.table::set(match, NULL, "radical", NULL)

    fixed <- data.table::data.table(
        character = c(
            "\u4e00", "\u4e8c", "\u4e09", "\u56db", "\u4e94",
            "\u516d", "\u4e03", "\u516b", "\u4e5d", "\u5341"
        ),
        stroke = 1:10
    )

    # NOTE: make CRAN check happy
    i.stroke <- stroke_wuge <- NULL

    match[fixed, on = "character", stroke_wuge := i.stroke]

    data.table::set(match, NULL, "pinyin", gsub("\\|.+$", "", match$pinyin))

    data.table::setcolorder(match, c("index", "character", "stroke", "pinyin"))
    data.table::setcolorder(match, c(setdiff(names(match), "stroke_wuge")))

    match
}

#' Get WuGe information of Chinese names
#'
#' `sim2tra()` uses the dictionary `STCharacter.txt` distributed via
#' [`OpenCC`](https://opencc.byvoid.com/) to convert Simplified Chinese to
#' Traditional Chinese.
#'
#' @param xing A character vector of Chinese characters indicating the last
#'        names.
#'
#' @param ming A character vector of Chinese characters indicating the first
#'        names.
#'
#' @param to_trad If `TRUE`, input characters are converted to Traditional
#'        Chinese using [sim2trad] before calculation. If [sim2trad] does not
#'        give correct results, you may want to set `to_trad` to FALSE and
#'        directly specify Tradition Chinese characters as input. Default:
#'        `TRUE`
#'
#' @examples
#' get_wuge(c("张", "李"), c("三", "四五"))
#'
#' @export
get_wuge <- function(xing, ming, to_trad = TRUE) {
    if (length(xing) != length(ming)) {
        if (length(xing) == 1L) {
            last <- rep(xing, length(ming))
        } else if (length(ming) == 1L) {
            first <- rep(ming, length(xing))
        } else {
            stop(sprintf(
                "`xing` and `ming` should have the same length. But `length(last)` = %i while `length(first)` = %i.",
                length(xing), length(ming)
            ))
        }
    }

    dt_xing <- get_wuge_char_data(xing, to_trad = to_trad)
    dt_ming <- get_wuge_char_data(ming, to_trad = to_trad)

    # calculate wuge
    wuge <- dt_xing[, by = "index", list(
        tian = c(stroke_wuge[1L] + 1L, sum(stroke_wuge))[c(.N == 1L, .N != 1L)]
    )]
    ## Ren
    data.table::set(
        wuge, NULL, "ren",
        dt_xing[, by = "index", .SD[.N], .SDcols = "stroke_wuge"]$stroke_wuge +
            dt_ming[, by = "index", .SD[1L], .SDcols = "stroke_wuge"]$stroke_wuge
    )
    ## Di
    # NOTE: to make CRAN checks happy
    stroke_wuge <- .N <- NULL
    data.table::set(
        wuge, NULL, "di",
        dt_ming[, by = "index", {
            c(stroke_wuge[.N] + 1L, sum(stroke_wuge))[c(.N == 1L, .N != 1L)]
        }]$V1
    )
    ## Zong
    data.table::set(
        wuge, NULL, "zong",
        dt_xing[, by = "index", sum(stroke_wuge)]$V1 +
            dt_ming[, by = "index", sum(stroke_wuge)]$V1
    )
    ## Wai
    data.table::set(wuge, NULL, "wai", wuge$tian + wuge$di - wuge$ren)
    data.table::setcolorder(wuge, setdiff(names(wuge), "zong"))

    # get shuli
    shuli <- data.table::melt.data.table(wuge,
        id.vars = "index", variable.factor = FALSE,
        variable.name = "wuge", value.name = "num"
    )
    data.table::set(shuli, NULL, "wuxing", get_wuxing(shuli$num))
    data.table::setorderv(shuli, "index")

    # get sancai
    sancai <- data.table::data.table(
        index = wuge$index,
        wuxing = shuli[, by = "index", str_join(wuxing[1:3])]$V1
    )
    sancai <- dict_sancai()[, .SD, .SDcols = c("wuxing", "jixiong", "description")][
        sancai, on = "wuxing"
    ]
    data.table::setcolorder(sancai, c("index", "wuxing"))

    # get special sancai
    sancai <- dict_special_sancai()[sancai, on = "wuxing"]
    data.table::setnames(sancai,
        c("i.description", "description"),
        c("description", "description_special")
    )
    data.table::setcolorder(sancai, c("index", "wuxing", "jixiong", "description"))

    shuli <- dict_shuli()[shuli, on = "num"]
    data.table::setcolorder(shuli, c("index", "wuge", "wuxing", "num"))

    # get wuge in wuxing style
    wuxing <- data.table::as.data.table(
        c(list(index = wuge$index), lapply(wuge[, -1L], get_wuxing))
    )
    # get wuge in mode style
    wuge_mod <- data.table::as.data.table(
        c(list(index = wuge$index), lapply(wuge[, -1L], `%%`, 10L))
    )

    # get luck
    ## Base
    base <- dict_luck_base()[wuxing, on = c("ren", "di")]
    data.table::set(base, NULL, c("tian", "wai", "zong"), NULL)
    data.table::setcolorder(base, "index")
    ## Success
    success <- dict_luck_success()[wuxing, on = c("ren", "tian")]
    data.table::set(success, NULL, c("di", "wai", "zong"), NULL)
    data.table::setcolorder(success, "index")
    ## Social
    social <- dict_luck_social()[wuge_mod, on = c("ren", "wai")]
    data.table::set(social, NULL, c("tian", "di", "zong"), NULL)
    data.table::setcolorder(social, "index")
    ## Health
    health <- dict_luck_health()[wuge_mod, on = c("tian", "ren", "di")]
    data.table::set(health, NULL, c("wai", "zong"), NULL)
    data.table::setcolorder(health, "index")

    # calculate scores
    # ref: https://github.com/whmnoe4j/Calendar/blob/master/app/Services/NameTest.php
    score_shuli <- data.table::data.table(
        index = shuli$index,
        wuge = shuli$wuge,
        score = with(shuli,
            data.table::fcase(
                jixiong == "\u5927\u5409"            , 100,
                jixiong == "\u5409"                  , 90,
                jixiong == "\u534a\u5409"            , 80,
                jixiong == "\u534a\u5409\u534a\u51f6", 60,
                jixiong == "\u51f6"                  , 40,
                jixiong == "\u5927\u51f6"            , 30
            )
        )
    )
    data.table::set(score_shuli, NULL, "score",
        with(score_shuli,
            data.table::fcase(
                wuge == "tian", score * 0.05,
                wuge == "di"  , score * 0.20,
                wuge == "ren" , score * 0.50,
                wuge == "wai" , score * 0.05,
                wuge == "zong", score * 0.20
            )
        )
    )
    data.table::set(score_shuli, NULL, "wuge", NULL)
    score_shuli <- score_shuli[, lapply(.SD, sum), by = "index"]

    score_sancai <- data.table::data.table(
        index = sancai$index,
        score = with(sancai,
            data.table::fcase(
                jixiong == "\u5927\u5409"            , 100,
                jixiong == "\u5409"                  , 95,
                jixiong == "\u4e2d\u5409"            , 85,
                jixiong == "\u5409\u591a\u4e8e\u51f6", 75,
                jixiong == "\u559c\u51f6\u53c2\u534a", 60,
                jixiong == "\u51f6\u591a\u4e8e\u5409", 45,
                jixiong == "\u5927\u51f6"            , 30
            )
        )
    )

    data.table::set(score_shuli, NULL, "score",
        round(score_shuli$score * 0.75 + score_sancai$score * 0.25, 1L)
    )

    structure(
        list(
            score = score_shuli,
            xing = dt_xing,
            ming = dt_ming,
            sancai = sancai,
            shuli = shuli,
            base = base,
            success = success,
            social = social,
            health = health
        ),
        class = "WuGe"
    )
}

get_wuxing <- function(stroke) {
    mod <- stroke %% 10L
    data.table::fcase(
        mod == 1L | mod == 2L, "\u6728",
        mod == 3L | mod == 4L, "\u706b",
        mod == 5L | mod == 6L, "\u571f",
        mod == 7L | mod == 8L, "\u91d1",
        mod == 9L | mod == 0L, "\u6c34"
    )
}
