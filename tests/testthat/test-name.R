test_that("new name calculation works", {
    expect_error(name(c("王", "司马"), num = 0))

    expect_error(name(c("王", "司马"), fixed = -1))

    expect_error(name(c("王", "司马"), fixed = 0))

    expect_equal(
        names(name(c("王", "司马"), max = 30, min = 3, fixed = c(0, 4))),
        c("xing", "ming", "score", "shuli", "sancai", "luck")
    )

    expect_s3_class(nm <- name(c("王", "司马"), num = 1, max = 4), "WuGeName")

    expect_equal(
        names(nm$xing),
        c("index", "character", "stroke", "pinyin", "traditional", "stroke_wuge")
    )

    expect_equal(
        names(nm$ming),
        c("index", "id", "character", "stroke", "pinyin", "traditional", "stroke_wuge")
    )

    expect_equal(
        names(nm$score),
        c("index", "id", "tian", "ren", "di", "wai", "zong", "sancai",
            "score_tian", "score_ren", "score_di", "score_wai", "score_zong",
            "score_shuli", "score_sancai", "score_total"
        )
    )

    expect_equal(
        names(nm$shuli),
        c(
            "index", "id", "wuge", "num", "wuxing", "score", "jixiong",
            "brief", "short", "indication", "foundation", "family", "health",
            "future", "fortune", "description"
        )
    )

    expect_equal(
        names(nm$sancai),
        c("index", "id", "wuxing", "jixiong", "score", "brief", "description", "detail", "description_special")
    )

    expect_equal(
        names(nm$luck),
        c("base", "success", "social", "health")
    )

    expect_equal(
        names(nm$luck$base),
        c("index", "id", "ren", "di", "jixiong", "description")
    )

    expect_equal(
        names(nm$luck$success),
        c("index", "id", "ren", "tian", "jixiong", "description")
    )

    expect_equal(
        names(nm$luck$social),
        c("index", "id", "ren", "wai", "jixiong", "description")
    )

    expect_equal(
        names(nm$luck$health),
        c("index", "id", "tian", "ren", "di", "description")
    )
})
