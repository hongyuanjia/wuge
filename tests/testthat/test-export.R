test_that("Can export WuGe", {
    skip_on_cran()

    expect_error(export("a"))

    f <- tempfile(fileext = ".png")
    expect_type(img <- export(wuge("张", "三"), f, width = 10.5, height = 27, dpi = 30), "character")
    expect_true(file.exists(img))

    f <- tempfile(fileext = ".xlsx")
    expect_type(xlsx <- export(name("张", max = 8), f, width = 10.5, height = 27, dpi = 30), "character")
    expect_true(file.exists(xlsx))
})
