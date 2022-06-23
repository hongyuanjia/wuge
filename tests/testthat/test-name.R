test_that("new name calculation works", {
    expect_equal(
        names(cal_strokes(c("王", "司马"), max_stroke = 30)),
        c("xing", "ming_strokes")
    )

    expect_equal(
        names(cal_strokes(c("王", "司马"), max_stroke = 30)$xing),
        c("index", "character", "stroke", "pinyin", "traditional", "stroke_wuge")
    )

    expect_equal(
        names(cal_strokes(c("王", "司马"), num_char = 1, max_stroke = 4)$ming_strokes),
        c("index", "stroke_wuge_1", "tian", "ren", "di", "wai", "zong", "sancai",
            "jixiong_tian", "jixiong_ren", "jixiong_di", "jixiong_wai", "jixiong_zong", "jixiong_sancai",
            "score_tian", "score_ren", "score_di", "score_wai", "score_zong",
            "score_wuge", "score_sancai", "score_total"
        )
    )
})
