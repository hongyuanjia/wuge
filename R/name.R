get_avail_strokes <- function(min_stroke = NULL, max_stroke = NULL) {
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

cal_strokes <- function(xing, num_char = 2L, min_stroke = NULL, max_stroke = NULL,
                        allow_general = FALSE) {
    assert_flag(allow_general)

    dt_xing <- get_wuge_char_data(xing)

    # subset Simplified Chinese strokes
    wuge_strokes <- get_avail_strokes(min_stroke, max_stroke)

    num_char <- assert_count(num_char, 1L)
    if (num_char >= 4L) {
        stop(sprintf("`n_char` should be less than 4L but input is '%s'.", num_char))
    }

    dt_ming <- do.call(data.table::CJ, replicate(num_char, list(wuge_strokes)))

    # calculate wuge for all possible combinations
    wuge <- data.table::rbindlist(
        Map(
            get_wuge_val,
            stroke_xing = split.default(dt_xing$stroke_wuge, dt_xing$index),
            stroke_ming = replicate(length(xing), dt_ming, simplify = FALSE),
            len_xing = nchar(xing),
            len_ming = num_char
        ),
        idcol = "index"
    )
    data.table::set(wuge, NULL, "index", as.integer(wuge$index))
    data.table::set(wuge, NULL, "id", seq.int(nrow(wuge)))
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
                .subset(jixiong, tian),
                .subset(jixiong, ren),
                .subset(jixiong, di),
                .subset(jixiong, wai),
                .subset(jixiong, zong)
            )
        })
    )

    # only select entries that exceeds the threshold
    thld_wuge <- if (allow_general) 60L else 80L
    wuge_sel <- wuge[
        with(wuge,
            score_ren >= thld_wuge &
                score_di >= thld_wuge &
                score_wai >= thld_wuge &
                score_zong >= thld_wuge
        )
    ]

    # get sancai in wuxing style
    data.table::set(wuge_sel, NULL, "sancai",
        with(wuge_sel,
            paste0(
                get_stroke_wuxing(tian),
                get_stroke_wuxing(ren),
                get_stroke_wuxing(di)
            )
        )
    )

    # get sancai jixiong
    data.table::set(wuge_sel, NULL, c("jixiong_sancai", "score_sancai"),
        {
            ind <- data.table::chmatch(wuge_sel$sancai, dict_sancai()$wuxing)
            list(dict_sancai()$jixiong[ind], dict_sancai()$score[ind])
        }
    )

    thld_sancai <- if (allow_general) 60L else 75L
    # NOTE: to make CRAN checks happy
    score_sancai <- NULL
    wuge_sancai_sel <- wuge_sel[score_sancai >= thld_sancai]

    # calculate total score
    data.table::set(wuge_sancai_sel, NULL, "score_wuge",
        with(wuge_sancai_sel,
            score_tian * 0.05 + score_di * 0.20 + score_ren * 0.50 +
                score_wai * 0.05 + score_zong * 0.20
        )
    )
    data.table::set(wuge_sancai_sel, NULL, "score_total",
        with(wuge_sancai_sel, round(score_wuge * 0.75 + score_sancai * 0.25, 1L))
    )
    data.table::set(wuge_sancai_sel, NULL, "score_wuge", round(wuge_sancai_sel$score_wuge, 1L))

    # order by scores
    data.table::setorderv(wuge_sancai_sel,
        c("index", "score_total", "score_wuge", "score_sancai"),
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
            "score_wuge", "score_sancai", "score_total"
        )
    )

    list(
        xing = dt_xing,
        ming_strokes = wuge_sancai_sel
    )
}

#' @param strokes An integer vector of WuGe strokes
#' @noRd
get_char_data_from_stroke <- function(strokes) {
    input <- data.table::data.table(
        index = seq_len(length(strokes)),
        stroke_wuge = strokes,
        key = "stroke_wuge"
    )

    dict_fullchar()[input, on = "stroke_wuge"][,
        lapply(.SD, list), by = c("index", "stroke_wuge")]
}
