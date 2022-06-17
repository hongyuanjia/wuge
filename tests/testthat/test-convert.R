test_that("character conversion from simplifed to traditional works", {
    expect_equal(
        sim2trad(c("个十", "百千", "万")),
        c("个十" = "個十", "百千" = "百千", "万" = "萬")
    )

    expect_equal(
        sim2trad(c("个十", "百千", "万"), all = TRUE),
        list(
            "个十" = c("個十", "箇十"),
            "百千" = c("百千", "百韆"),
            "万" = c("萬", "万")
        )
    )
})
