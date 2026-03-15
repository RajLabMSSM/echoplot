test_that("guess_genomic_units detects POS (integer-like) units", {

    df <- data.frame(POS = c(100000, 200000, 300000), y = c(1, 2, 3))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = POS, y = y)) +
        ggplot2::geom_point()
    res <- echoplot:::guess_genomic_units(gg = gg, verbose = FALSE)
    testthat::expect_equal(res, "POS")
})

test_that("guess_genomic_units detects Mb (decimal) units", {

    df <- data.frame(Mb = c(1.23456, 1.56789, 1.89012), y = c(1, 2, 3))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = Mb, y = y)) +
        ggplot2::geom_point()
    res <- echoplot:::guess_genomic_units(gg = gg, verbose = FALSE)
    testthat::expect_equal(res, "Mb")
})

test_that("guess_genomic_units uses custom decimals_default", {

    df <- data.frame(x = c(1.23456, 1.56789), y = c(1, 2))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point()
    res <- echoplot:::guess_genomic_units(
        gg = gg,
        decimals_default = "Kb",
        verbose = FALSE
    )
    testthat::expect_equal(res, "Kb")
})
