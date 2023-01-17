test_that("get_char_data() works", {
    expect_equal(
        names(char_data(c("十千", "万"))),
        c("index", "character", "stroke", "pinyin", "radical")
    )
})

test_that("stroke counting works", {
    expect_equal(
        stroke(c("十百", "千万")),
        c("十百" = 8, "千万" = 6L)
    )
})

test_that("pinyin searching works", {
    expect_equal(
        pinyin(c("十百", "千万")),
        c("十百" = "shí bǎi", "千万" = "qiān wàn")
    )
})
