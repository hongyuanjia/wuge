test_that("wuge value calculation works", {
    expect_equal(
        wuge_val(13, c(3, 2), 1, 2),
        list(tian = 14L, ren = 16L, di = 5L, wai = 3L, zong = 18L)
    )
})

test_that("wuge character data extraction works", {
    expect_equal(
        wuge_char_data(c("十百", "千万")),
        data.table(
            index = c(1L, 1L, 2L, 2L),
            character = c("十", "百", "千", "万"),
            stroke = c(2L, 6L, 3L, 3L),
            pinyin = c("shí", "bǎi", "qiān", "wàn"),
            traditional = c("十", "百", "千", "萬"),
            stroke_wuge = c(10L, 6L, 3L, 15L)
        )
    )

    expect_equal(
        wuge_char_data(c("十百", "千万"), to_trad = FALSE),
        data.table(
            index = c(1L, 1L, 2L, 2L),
            character = c("十", "百", "千", "万"),
            stroke = c(2L, 6L, 3L, 3L),
            pinyin = c("shí", "bǎi", "qiān", "wàn"),
            traditional = c("十", "百", "千", "万"),
            stroke_wuge = c(10L, 6L, 3L, 3L)
        )
    )
})

test_that("wuge calculation works", {
    wuge <- data.table::setDT(wuge_val(13, c(3, 2)))

    expect_equal(
        wuge_score(wuge),
        data.table::data.table(
            index = 1L, tian = 14, ren = 16, di = 5, wai = 3, zong = 18, sancai = "火土土",
            score_tian = 45, score_ren = 98, score_di = 98, score_wai = 98,
            score_zong = 90, score_shuli = 93.75, score_sancai = 98, score_total = 94.8
        )
    )

    expect_equal(
        names(wuge_shuli(wuge)),
        c("index", "wuge", "num", "wuxing", "score",
            "jixiong", "brief", "short", "indication", "foundation", "family", "health",
            "future", "fortune", "description"
        )
    )

    expect_equal(
        names(wuge_sancai(wuge)),
        c(
            "index", "wuxing", "jixiong", "score", "brief", "description",
            "detail", "description_special"
        )
    )

    expect_equal(
        names(wuge_luck(wuge)),
        c("base", "success", "social", "health")
    )

    expect_s3_class(wuge <- wuge(c("王", "司马"), "一"), "WuGe")

    expect_equal(
        names(wuge),
        c("xing", "ming", "score", "shuli", "sancai", "luck")
    )

    expect_equal(
        names(wuge$xing),
        c("index", "character", "stroke", "pinyin", "traditional", "stroke_wuge")
    )

    expect_equal(
        names(wuge$ming),
        c("index", "character", "stroke", "pinyin", "traditional", "stroke_wuge")
    )

    expect_equal(
        names(wuge$score),
        c("index", "tian", "ren", "di", "wai", "zong", "sancai",
            "score_tian", "score_ren", "score_di", "score_wai", "score_zong",
            "score_shuli", "score_sancai", "score_total"
        )
    )

    expect_equal(
        names(wuge$shuli),
        c(
            "index", "wuge", "num", "wuxing", "score", "jixiong",
            "brief", "short", "indication", "foundation", "family", "health",
            "future", "fortune", "description"
        )
    )

    expect_equal(
        names(wuge$sancai),
        c("index", "wuxing", "jixiong", "score", "brief", "description", "detail", "description_special")
    )

    expect_equal(
        names(wuge$luck),
        c("base", "success", "social", "health")
    )

    expect_equal(
        names(wuge$luck$base),
        c("index", "ren", "di", "jixiong", "description")
    )

    expect_equal(
        names(wuge$luck$success),
        c("index", "ren", "tian", "jixiong", "description")
    )

    expect_equal(
        names(wuge$luck$social),
        c("index", "ren", "wai", "jixiong", "description")
    )

    expect_equal(
        names(wuge$luck$health),
        c("index", "tian", "ren", "di", "description")
    )
})
