avail_strokes <- function(min_stroke = NULL, max_stroke = NULL) {
    if (is.null(min_stroke)) {
        min_stroke <- 1L
    } else {
        assert_count(min_stroke)
    }
    if (is.null(max_stroke)) {
        max_stroke <- Inf
    } else {
        assert_count(max_stroke)

        if (min_stroke > max_stroke) {
            stop(sprintf(
                "`max_stroke` should be greater than `min_stroke` (`%s`) but input is `%s`.",
                min_stroke, max_stroke
            ))
        }
    }

    unique(dict_fullchar()$stroke_wuge[
        data.table::between(dict_fullchar()$stroke, min_stroke, max_stroke)
    ])
}

#' Calculate character stroke combinations that meet specified requirements
#'
#' @param xing A character vector of Chinese characters indicating the last
#'        names.
#'
#' @param num_char An integer indicating number of characters in the given
#'        names. Default: `2L`.
#'
#' @param min_stroke,max_stroke An integer indicating the minimum and maximum
#'        stroke numbers in the given names. `NULL` means no restrictions.
#'        Default: `NULL`.
#'
#' @param fixed_stroke An integer vector that has a length of `num_char`
#'        indicating the fixed stroke number in the given names. `0` means do
#'        not fix. E.g. if `num_char` is `2L`, `c(0, 10)` means that the second
#'        character in the given names should has a stroke number of `10`, while
#'        there is no fixed stroke number for the first character. Default:
#'        `NULL`.
#'
#' @param min_shuli,max_sancai A string indicating the minimum luck for WuGe and
#'        SanCai. Default: `\u5409`.
#'
#' @noRd
cal_strokes <- function(xing, num_char = 2L, min_stroke = NULL, max_stroke = NULL,
                        fixed_stroke = NULL, min_shuli = "\u5409", min_sancai = min_shuli) {
    num_char <- assert_count(num_char, 1L)
    if (num_char >= 4L) {
        stop(sprintf("`num_char` should be less than 4L but input is '%s'.", num_char))
    }

    if (is.null(fixed_stroke)) {
        # subset Simplified Chinese strokes
        strokes <- replicate(num_char, list(avail_strokes(min_stroke, max_stroke)))
    } else {
        if (!is.numeric(fixed_stroke) || !length(fixed_stroke)) {
            stop("`fixed_stroke` should be an integer vector.")
        }

        fixed_stroke[is.na(fixed_stroke)] <- 0
        if (any(fixed_stroke < 0 | fixed_stroke != trunc(fixed_stroke))) {
            stop("`fixed_stroke` should be an non-negative integer vector.")
        }

        if (length(fixed_stroke) != num_char) {
            stop(sprintf(
                "`fixed_stroke` should have a length that equals to `num_char` (%i) but `%i` is found.",
                num_char, length(fixed_stroke)
            ))
        }

        fixed_stroke <- as.integer(fixed_stroke)

        strokes <- as.list(fixed_stroke)
        if (any(free <- fixed_stroke == 0L)) {
            strokes[free] <- list(avail_strokes(min_stroke, max_stroke))
        }
    }
    dt_ming <- do.call(data.table::CJ, strokes)

    dt_xing <- wuge_char_data(xing)

    # calculate wuge for all possible combinations
    wuge <- data.table::rbindlist(
        Map(
            wuge_val,
            stroke_xing = split.default(dt_xing$stroke_wuge, dt_xing$index),
            stroke_ming = replicate(length(xing), dt_ming, simplify = FALSE),
            len_xing = nchar(xing),
            len_ming = num_char
        ),
        idcol = "index"
    )

    data.table::set(wuge, NULL, "index", as.integer(wuge$index))
    data.table::set(wuge, NULL, "id", seq.int(nrow(wuge)))

    # NOTE: to make CRAN checks happy
    tian <- ren <- di <- wai <- zong <- NULL
    # remove wuge that exceeds the maximum number 81
    wuge <- wuge[tian <= 81L & ren <= 81L & di <= 81L & wai <= 81L & zong <= 81L]

    data.table::set(wuge, NULL,
        c("score_tian", "score_ren", "score_di", "score_wai", "score_zong",
          "jixiong_tian", "jixiong_ren", "jixiong_di", "jixiong_wai", "jixiong_zong"
        ),
        with(wuge, {
            shuli <- dict_shuli()
            score <- shuli$score
            jixiong <- shuli$jixiong
            list(
                .subset(score, tian),
                .subset(score, ren),
                .subset(score, di),
                .subset(score, wai),
                .subset(score, zong),
                # NOTE: cannot use .subset here
                # .subset(factor, i) will coerce factors to integers
                jixiong[tian], jixiong[ren], jixiong[di], jixiong[wai], jixiong[zong]
            )
        })
    )

    assert_choice(min_shuli, levels(dict_shuli()$jixiong), null_ok = FALSE)
    assert_choice(min_sancai, levels(dict_shuli()$jixiong), null_ok = FALSE)

    # NOTE: to make CRAN checks happy
    jixiong_ren <- jixiong_di <- jixiong_wai <- jixiong_zong <- NULL
    # only select entries that exceeds the threshold
    wuge_sel <- wuge[jixiong_ren >= min_shuli & jixiong_di >= min_shuli & jixiong_wai >= min_shuli & jixiong_zong >= min_shuli]

    # get sancai in wuxing style
    data.table::set(wuge_sel, NULL, "sancai",
        with(wuge_sel, paste0(stroke_wuxing(tian), stroke_wuxing(ren), stroke_wuxing(di)))
    )

    # get sancai jixiong
    data.table::set(wuge_sel, NULL, c("jixiong_sancai", "score_sancai"),
        {
            ind <- data.table::chmatch(wuge_sel$sancai, dict_sancai()$wuxing)
            list(dict_sancai()$jixiong[ind], dict_sancai()$score[ind])
        }
    )

    # NOTE: to make CRAN checks happy
    jixiong_sancai <- NULL
    wuge_sancai_sel <- wuge_sel[jixiong_sancai >= min_sancai]

    # NOTE: to make CRAN checks happy
    score_tian <- score_ren <- score_di <- score_wai <- score_zong <- NULL
    # calculate total score
    data.table::set(wuge_sancai_sel, NULL, "score_shuli",
        with(wuge_sancai_sel, wuge_score_shuli(score_tian, score_di, score_ren, score_wai, score_zong))
    )
    data.table::set(wuge_sancai_sel, NULL, "score_total",
        wuge_score_total(wuge_sancai_sel$score_shuli, wuge_sancai_sel$score_sancai)
    )
    data.table::set(wuge_sancai_sel, NULL, "score_shuli", round(wuge_sancai_sel$score_shuli, 1L))

    # order by scores
    data.table::setorderv(wuge_sancai_sel,
        c("index", "score_total", "score_shuli", "score_sancai"),
        c(1L, -1L, -1L, -1L)
    )

    # get actual strokes
    data.table::set(wuge_sancai_sel, NULL,
        names(dt_ming),
        {
            ind <- wuge_sancai_sel$id
            n <- length(xing)
            lapply(dt_ming, function(val) .subset(rep.int(val, n), ind))
        }
    )
    data.table::setcolorder(wuge_sancai_sel, c("index", names(dt_ming)))
    data.table::setnames(wuge_sancai_sel,
        names(dt_ming),
        paste("stroke_wuge", substring(names(dt_ming), 2L), sep = "_")
    )

    # clean up
    data.table::set(wuge_sancai_sel, NULL, "id", NULL)

    data.table::setcolorder(wuge_sancai_sel,
        c(
            names(wuge_sancai_sel)[seq_len(length(dt_ming) + 1L)],
            "tian", "ren", "di", "wai", "zong", "sancai",
            "jixiong_tian", "jixiong_ren", "jixiong_di", "jixiong_wai",
            "jixiong_zong", "jixiong_sancai",
            "score_tian", "score_ren", "score_di", "score_wai", "score_zong",
            "score_shuli", "score_sancai", "score_total"
        )
    )

    list(
        xing = dt_xing,
        ming_strokes = wuge_sancai_sel
    )
}

#' Get Chinese characters that meet WuGe strokes
#'
#' @param strokes An integer vector of WuGe strokes
#'
#' @param common If `TRUE`, only common Chinese characters will be used. Default:
#'        `TRUE`
#' @noRd
char_data_from_stroke <- function(strokes, common = TRUE) {
    assert_flag(common)
    input <- data.table::data.table(
        index = seq_len(length(strokes)),
        stroke_wuge = strokes,
        key = "stroke_wuge"
    )

    dict <- dict_fullchar()
    if (common) {
        path <- system.file("extdata/char_common.csv", package = "wuge")
        DICT_CHAR_COMMON <- data.table::fread(path, encoding = "UTF-8")
        dict <- dict[J(DICT_CHAR_COMMON$character), on = "character"]
    }

    res <- dict[input, on = "stroke_wuge"][, lapply(.SD, list), by = c("index", "stroke_wuge")]
    data.table::setorderv(res, "index")
    res
}
