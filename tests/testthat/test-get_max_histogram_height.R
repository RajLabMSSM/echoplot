test_that("get_max_histogram_height works with histogram", {

    df <- data.frame(x = rnorm(100))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(bins = 10)
    res <- echoplot:::get_max_histogram_height(gg = gg, verbose = FALSE)
    testthat::expect_true(is.numeric(res))
    testthat::expect_true(res > 0)
})

test_that("get_max_histogram_height rounds when round_to given", {

    df <- data.frame(x = rnorm(100))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(bins = 10)
    res <- echoplot:::get_max_histogram_height(
        gg = gg,
        round_to = 5,
        verbose = FALSE
    )
    testthat::expect_true(is.numeric(res))
    testthat::expect_equal(res %% 5, 0)
})

test_that("get_max_histogram_height works with bar chart", {

    df <- data.frame(x = c("A", "A", "A", "B", "B"))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_bar()
    res <- echoplot:::get_max_histogram_height(gg = gg, verbose = FALSE)
    testthat::expect_true(is.numeric(res))
    testthat::expect_equal(res, 3)
})
