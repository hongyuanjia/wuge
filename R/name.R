avail_strokes <- function(min = NULL, max = NULL, common = FALSE) {
    if (is.null(min)) {
        min <- 1L
    } else {
        assert_count(min)
    }
    if (is.null(max)) {
        max <- Inf
    } else {
        assert_count(max)

        if (min > max) {
            stop(sprintf(
                "`max` should be greater than `min` (`%s`) but input is `%s`.",
                min, max
            ))
        }
    }

    assert_flag(common)
    if (!common) {
        unique(dict_fullchar()$stroke_wuge[
            data.table::between(dict_fullchar()$stroke, min, max)
        ])
    } else {
        unique(dict_fullchar()$stroke_wuge[
            dict_fullchar()$character %in% dict_char_common()$character &
            data.table::between(dict_fullchar()$stroke, min, max)
        ])
    }
}

#' Calculate character stroke combinations that meet specified requirements
#'
#' @param xing A character vector of Chinese characters indicating the last
#'        names.
#'
#' @param num An integer indicating number of characters in the given names.
#'        Default: `2L`.
#'
#' @param min,max An integer indicating the minimum and maximum stroke numbers
#'        in the given names. `NULL` means no restrictions. Default: `NULL`.
#'
#' @param fixed An integer vector that has a length of `num` indicating the
#'        fixed stroke number in the given names. `0` means do not fix. E.g. if
#'        `num` is `2L`, `c(0, 10)` means that the second character in the
#'        given names should has a stroke number of `10`, while there is no
#'        fixed stroke number for the first character. Default: `NULL`.
#'
#' @param shuli,sancai A string indicating the minimum luck for WuGe and
#'        SanCai. Default: `"\u5409"`.
#'
#' @param common If `TRUE`, only around 3500 common Chinese characters will be
#'        used. Default: `TRUE`
#'
#' @return An `WuGeName` object.
#'
#' @export
name <- function(xing, num = 2L, min = NULL, max = NULL, fixed = NULL,
                 shuli = "\u5409", sancai = shuli, common = TRUE) {
    num <- assert_count(num, 1L)
    if (num >= 4L) {
        stop(sprintf("`num` should be less than 4L but input is '%s'.", num))
    }

    if (is.null(fixed)) {
        # subset Simplified Chinese strokes
        strokes <- replicate(num, list(avail_strokes(min, max, common)))
    } else {
        if (!is.numeric(fixed) || !length(fixed)) {
            stop("`fixed` should be an integer vector.")
        }

        fixed[is.na(fixed)] <- 0
        if (any(fixed < 0 | fixed != trunc(fixed))) {
            stop("`fixed` should be an non-negative integer vector.")
        }

        if (length(fixed) != num) {
            stop(sprintf(
                "`fixed` should have a length that equals to `num` (%i) but `%i` is found.",
                num, length(fixed)
            ))
        }

        fixed <- as.integer(fixed)

        strokes <- as.list(fixed)
        if (any(free <- fixed == 0L)) {
            strokes[free] <- list(avail_strokes(min, max, common))
        }
    }
    dt_ming <- do.call(data.table::CJ, strokes)

    dt_xing <- wuge_char_data(xing)

    # calculate wuge for all possible combinations
    score <- data.table::rbindlist(
        Map(
            wuge_val,
            stroke_xing = split.default(dt_xing$stroke_wuge, dt_xing$index),
            stroke_ming = replicate(length(xing), dt_ming, simplify = FALSE),
            len_xing = nchar(xing),
            len_ming = num
        ),
        idcol = "index"
    )

    data.table::set(score, NULL, "index", as.integer(score$index))
    data.table::set(score, NULL, "id", seq.int(nrow(score)))

    # NOTE: to make CRAN checks happy
    tian <- ren <- di <- wai <- zong <- NULL
    # remove wuge that exceeds the maximum number 81
    score <- score[tian <= 81L & ren <= 81L & di <= 81L & wai <= 81L & zong <= 81L]

    data.table::set(score, NULL,
        c("score_tian", "score_ren", "score_di", "score_wai", "score_zong",
          "jixiong_tian", "jixiong_ren", "jixiong_di", "jixiong_wai", "jixiong_zong"
        ),
        with(score, {
            shuli <- dict_shuli()
            sc <- shuli$score
            jx <- shuli$jixiong
            list(
                .subset(sc, tian), .subset(sc, ren), .subset(sc, di), .subset(sc, wai), .subset(sc, zong),
                # NOTE: cannot use .subset here
                # .subset(factor, i) will coerce factors to integers
                jx[tian], jx[ren], jx[di], jx[wai], jx[zong]
            )
        })
    )

    assert_choice(shuli, levels(dict_shuli()$jixiong), null_ok = FALSE)
    assert_choice(sancai, levels(dict_sancai()$jixiong), null_ok = FALSE)

    # NOTE: to make CRAN checks happy
    jixiong_ren <- jixiong_di <- jixiong_wai <- jixiong_zong <- NULL
    # only select entries that exceeds the threshold
    score <- score[jixiong_ren >= shuli & jixiong_di >= shuli & jixiong_wai >= shuli & jixiong_zong >= shuli]

    # get sancai in wuxing style
    data.table::set(score, NULL, "sancai",
        with(score, paste0(stroke_wuxing(tian), stroke_wuxing(ren), stroke_wuxing(di)))
    )

    # get sancai jixiong
    data.table::set(score, NULL, c("jixiong_sancai", "score_sancai"),
        {
            sc <- dict_sancai()
            ind <- data.table::chmatch(score$sancai, sc$wuxing)
            list(sc$jixiong[ind], sc$score[ind])
        }
    )

    # NOTE: to make CRAN checks happy
    jixiong_sancai <- NULL
    min_sancai <- sancai
    score <- score[jixiong_sancai >= min_sancai]

    # NOTE: to make CRAN checks happy
    score_tian <- score_ren <- score_di <- score_wai <- score_zong <- NULL
    # calculate total score
    data.table::set(score, NULL, "score_shuli",
        with(score, wuge_score_shuli(score_tian, score_di, score_ren, score_wai, score_zong))
    )
    data.table::set(score, NULL, "score_total",
        wuge_score_total(score$score_shuli, score$score_sancai)
    )
    data.table::set(score, NULL, "score_shuli", round(score$score_shuli, 1L))

    # order by scores
    data.table::setorderv(score,
        c("index", "score_total", "score_shuli", "score_sancai"),
        c(1L, -1L, -1L, -1L)
    )

    # get actual strokes
    data.table::set(score, NULL,
        names(dt_ming),
        {
            ind <- score$id
            n <- length(xing)
            lapply(dt_ming, function(val) .subset(rep.int(val, n), ind))
        }
    )
    data.table::setcolorder(score, c("index", names(dt_ming)))
    data.table::setnames(score,
        names(dt_ming),
        paste("stroke_wuge", substring(names(dt_ming), 2L), sep = "_")
    )

    # reset id
    data.table::set(score, NULL, "id", data.table::rowidv(score, cols = "index"))

    # get sancai, shuli and luck data
    wuge <- score[, .SD, .SDcols = c("tian", "ren", "di", "wai", "zong")]
    sancai <- wuge_sancai(wuge)
    # set index and id back
    data.table::set(sancai, NULL, c("index", "id"), list(score$index, score$id))
    data.table::setcolorder(sancai, c("index", "id"))
    shuli <- wuge_shuli(wuge)
    # set index and id back
    data.table::set(shuli, NULL, c("index", "id"), list(score$index[shuli$index], score$id[shuli$index]))
    data.table::setcolorder(shuli, c("index", "id"))
    # set index and id back
    luck <- wuge_luck(wuge)
    for (dt in luck) {
        data.table::set(dt, NULL, c("index", "id"), list(score$index, score$id))
        data.table::setcolorder(dt, c("index", "id"))
    }

    # get character data
    col_ming <- paste("stroke_wuge", seq_len(num), sep = "_")
    chars <- char_data_from_stroke(common = common,
        unique(unlist(.subset(score, col_ming), FALSE, FALSE))
    )
    data.table::set(chars, NULL, c("index", "radical"), NULL)

    dt_ming <- score[, .SD, .SDcols = c("index", "id", col_ming)]
    dt_ming <- data.table::melt.data.table(dt_ming, c("index", "id"), value.name = "stroke_wuge")
    data.table::set(dt_ming, NULL, "variable", NULL)
    data.table::setorderv(dt_ming, c("index", "id"))
    dt_ming <- chars[dt_ming, on = "stroke_wuge"]
    data.table::setcolorder(dt_ming, c("index", "id", "character", "stroke", "pinyin", "traditional", "stroke_wuge"))

    # clean up
    col_sc <- c("index", "id",
        "tian", "ren", "di", "wai", "zong", "sancai",
        "score_tian", "score_ren", "score_di", "score_wai", "score_zong",
        "score_shuli", "score_sancai", "score_total"
    )
    data.table::set(score, NULL, setdiff(names(score), col_sc), NULL)
    data.table::setcolorder(score, col_sc)

    structure(
        list(
            xing = dt_xing, ming = dt_ming, score = score,
            shuli = shuli, sancai = sancai, luck = luck
        ),
        class = "WuGeName"
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
    # NOTE: to make CRAN checks happy
    J <- NULL
    if (common) dict <- dict[J(dict_char_common()$character), on = "character"]

    res <- dict[input, on = "stroke_wuge"][, lapply(.SD, list), by = c("index", "stroke_wuge")]
    data.table::setorderv(res, "index")
    res
}
