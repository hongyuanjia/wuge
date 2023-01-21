test_that("Can format WuGe", {
    skip_on_cran()

    expect_snapshot(print(wuge("张", "三")))
    expect_snapshot(print(name("张", max = 10)))

    expect_snapshot(format(wuge("张", "三"), width = 80))
    expect_snapshot(format(name("张", max = 10), width = 80))

    expect_snapshot(format(name(c("张", "司马"), max = 10, fixed = c(0, 4)), width = 80))
})
