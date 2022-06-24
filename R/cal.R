#' Calculate WuGe values based on strokes
#'
#' @param stroke_xing An integer vector of WuGe strokes for the family names.
#'
#' @param stroke_ming An integer vector of WuGe strokes for the given names.
#'
#' @param len_xing An integer indicating the input number of family names.
#'
#' @param len_ming An integer indicating the input number of given names
#'
#' @noRd
get_wuge_val <- function(stroke_xing, stroke_ming, len_xing = length(stroke_xing), len_ming = length(stroke_ming)) {
    sx <- psum(stroke_xing)
    sm <- psum(stroke_ming)

    # Tian
    tian <- if (len_xing == 1L) .subset2(stroke_xing, 1L) + 1L else sx

    # Ren
    ren <- .subset2(stroke_xing, len_xing) + .subset2(stroke_ming, 1L)

    # Di
    di <- if (len_ming == 1L) .subset2(stroke_ming, 1L) + 1L else sm

    # Zong
    zong <- sx + sm

    # Wai
    wai <- tian + di - ren

    list(
        tian = tian, ren = ren, di = di, wai = wai, zong = zong
    )
}

get_wuge_char_data <- function(char, to_trad = TRUE) {
    match <- split_char(char)

    # NOTE: right join is much faster than join and modify in place using `:=`
    if (!to_trad) {
        # treat input as traditional characters
        match <- dict_fullchar()[match, on = c("traditional" = "character")]

        # if any characters are not matched, retry with simplified characters
        if (length(miss <- which(is.na(match$stroke)))) {
            # NOTE: to make CRAN checks happy
            J <- NULL
            data.table::set(
                match, miss,
                c("character", "stroke", "pinyin", "radical", "stroke_wuge"),
                dict_fullchar()[
                    J(match$traditional[miss]), on = "character", .SD,
                    # NOTE: here use the strokes for simplified characters
                    .SDcols = c("character", "stroke", "pinyin", "radical", "stroke")
                ]
            )
        }
    } else {
        match <- dict_fullchar()[match, on = "character"]
    }

    # no need to include radical
    data.table::set(match, NULL, "radical", NULL)

    # only use the first pinyin
    data.table::set(match, NULL, "pinyin", gsub("\\|.+$", "", match$pinyin))

    data.table::setcolorder(match, c("index", "character", "stroke", "pinyin"))

    match
}

#' Get WuGe information of Chinese names
#'
#' `get_wuge()` gets the WuGe information of Chinese names.
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
            xing <- rep(xing, length(ming))
        } else if (length(ming) == 1L) {
            ming <- rep(ming, length(xing))
        } else {
            stop(sprintf(
                "`xing` and `ming` should have the same length. But `length(last)` = %i while `length(first)` = %i.",
                length(xing), length(ming)
            ))
        }
    }

    dt_xing <- get_wuge_char_data(xing, to_trad = to_trad)
    dt_ming <- get_wuge_char_data(ming, to_trad = to_trad)

    num_xing <- split.default(dt_xing$stroke_wuge, dt_xing$index)
    num_ming <- split.default(dt_ming$stroke_wuge, dt_ming$index)
    len_xing <- lengths(num_xing, use.names = FALSE)
    len_ming <- lengths(num_ming, use.names = FALSE)

    wuge <- data.table::rbindlist(
        Map(get_wuge_val, num_xing, num_ming, len_xing, len_ming),
        idcol = "index"
    )
    data.table::set(wuge, NULL, "index", as.integer(wuge$index))

    structure(
        list(
            xing = dt_xing,
            ming = dt_ming,
            score = get_wuge_score(wuge),
            sancai = get_wuge_sancai(wuge),
            shuli = get_wuge_shuli(wuge),
            luck = get_wuge_luck(wuge)
        ),
        class = "WuGe"
    )
}

get_stroke_wuxing <- function(stroke) {
    mod <- stroke %% 10L
    data.table::fcase(
        mod == 1L | mod == 2L, "\u6728",
        mod == 3L | mod == 4L, "\u706b",
        mod == 5L | mod == 6L, "\u571f",
        mod == 7L | mod == 8L, "\u91d1",
        mod == 9L | mod == 0L, "\u6c34"
    )
}

get_wuge_shuli <- function(wuge) {
    if (!"index" %in% names(wuge)) {
        data.table::set(wuge, NULL, "index", seq_len(nrow(wuge)))
    }

    shuli <- data.table::melt.data.table(wuge,
        id.vars = "index", variable.factor = FALSE,
        variable.name = "wuge", value.name = "num"
    )
    data.table::set(shuli, NULL, "wuxing", get_stroke_wuxing(shuli$num))
    data.table::set(shuli, NULL,
        c("score", "brief", "desc", "jixiong", "foundation", "family", "health", "future",
            "fortune", "desc_full"),
        with(dict_shuli(),
            {
                ind <- shuli$num
                list(
                    .subset(score, ind),
                    .subset(brief, ind),
                    .subset(desc, ind),
                    .subset(jixiong, ind),
                    .subset(foundation, ind),
                    .subset(family, ind),
                    .subset(health, ind),
                    .subset(future, ind),
                    .subset(fortune, ind),
                    .subset(desc_full, ind)
                )
            }
        )
    )
    data.table::setorderv(shuli, "index")
}

get_wuge_sancai <- function(wuge) {
    if (!"index" %in% names(wuge)) {
        data.table::set(wuge, NULL, "index", seq_len(nrow(wuge)))
    }

    sancai <- data.table::data.table(
        index = wuge$index,
        wuxing = with(
            wuge,
            paste0(get_stroke_wuxing(tian), get_stroke_wuxing(ren), get_stroke_wuxing(di))
        )
    )
    data.table::set(sancai, NULL, c("jixiong", "score", "description"),
        {
            dsancai <- dict_sancai()
            ind <- data.table::chmatch(sancai$wuxing, dsancai$wuxing)
            list(dsancai$jixiong[ind], dsancai$score[ind], dsancai$description[ind])
        }
    )

    # get special sancai
    data.table::set(sancai, NULL, "description_special",
        {
            dsancai <- dict_special_sancai()
            ind <- data.table::chmatch(sancai$wuxing, dsancai$wuxing)
            dsancai$description[ind]
        }
    )

    sancai
}

get_wuge_luck <- function(wuge) {
    if (!"index" %in% names(wuge)) {
        data.table::set(wuge, NULL, "index", seq_len(nrow(wuge)))
    }

    # get wuge in wuxing style
    wuxing <- data.table::as.data.table(
        c(list(index = wuge$index),
            lapply(
                .subset(wuge, c("tian", "ren", "di", "wai", "zong")),
                get_stroke_wuxing
            )
        )
    )
    # get wuge in mode style
    wuge_mod <- data.table::as.data.table(
        c(list(index = wuge$index),
            lapply(
                .subset(wuge, c("tian", "ren", "di", "wai", "zong")),
                `%%`, 10L
            )
        )
    )

    # Base
    base <- dict_luck_base()[wuxing, on = c("ren", "di")]
    data.table::set(base, NULL, c("tian", "wai", "zong"), NULL)
    data.table::setcolorder(base, "index")

    # Success
    success <- dict_luck_success()[wuxing, on = c("ren", "tian")]
    data.table::set(success, NULL, c("di", "wai", "zong"), NULL)
    data.table::setcolorder(success, "index")

    # Social
    social <- dict_luck_social()[wuge_mod, on = c("ren", "wai")]
    data.table::set(social, NULL, c("tian", "di", "zong"), NULL)
    data.table::setcolorder(social, "index")

    # Health
    health <- dict_luck_health()[wuge_mod, on = c("tian", "ren", "di")]
    data.table::set(health, NULL, c("wai", "zong"), NULL)
    data.table::setcolorder(health, "index")

    list(base = base, success = success, social = social, health = health)
}

get_wuge_score <- function(wuge) {
    score <- data.table::copy(wuge)

    if (!"index" %in% names(score)) {
        data.table::set(score, NULL, "index", seq_len(nrow(score)))
    }

    # ref: https://github.com/whmnoe4j/Calendar/blob/master/app/Services/NameTest.php
    data.table::set(score, NULL, "sancai",
        with(score, paste0(get_stroke_wuxing(tian), get_stroke_wuxing(ren), get_stroke_wuxing(di)))
    )
    data.table::set(score, NULL,
        c("score_tian", "score_ren", "score_di", "score_wai", "score_zong"),
        {
            all_scores <- dict_shuli()$score
            with(score,
                list(
                    all_scores[tian],
                    all_scores[ren],
                    all_scores[di],
                    all_scores[wai],
                    all_scores[zong]
                )
            )
        }
    )
    data.table::set(score, NULL, "score_wuge",
        with(score,
             score_tian * 0.05 + score_di * 0.20 + score_ren * 0.50 +
             score_wai * 0.05 + score_zong * 0.20
        )
    )
    data.table::set(score, NULL, "score_sancai",
        with(score, {
            all_sancai <- dict_sancai()
            all_sancai$score[data.table::chmatch(sancai, all_sancai$wuxing)]
        })
    )
    data.table::set(score, NULL, "score_total",
        round(score$score_wuge * 0.75 + score$score_sancai * 0.25, 1L)
    )

    data.table::setcolorder(score,
        c("index", "tian", "ren", "di", "wai", "zong", "sancai")
    )
    score
}
