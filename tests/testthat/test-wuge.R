test_that("wuge character data extraction works", {
    expect_equal(
        get_wuge_char_data(c("十百", "千万")),
        data.table(
            index = c(1L, 1L, 2L, 2L),
            character = c("十", "百", "千", "万"),
            stroke = c(2L, 6L, 3L, 3L),
            pinyin = c("shí", "bǎi", "qiān", "wàn"),
            traditional = c("十", "百", "千", "萬"),
            stroke_wuge = c(10L, 6L, 3L, 15L)
        )
    )
})

test_that("wuge calculation works", {
    expect_equal(
        names(get_wuge(c("王", "司马"), "一")),
        c("xing", "ming", "sancai", "shuli", "base", "success", "social", "health")
    )
})
