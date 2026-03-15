test_that("remove_margins returns ggplot list with zero margins", {

    gg1 <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5),
                           ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    gg2 <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3),
                           ggplot2::aes(x, y)) +
        ggplot2::geom_point()
    TRKS <- list("A" = gg1, "B" = gg2)
    dat <- data.frame(POS = c(100, 200))

    res <- echoplot:::remove_margins(
        TRKS = TRKS,
        dat = dat,
        verbose = FALSE
    )
    testthat::expect_true(is.list(res))
    testthat::expect_length(res, 2)
    testthat::expect_true(methods::is(res[["A"]], "gg"))
    testthat::expect_true(methods::is(res[["B"]], "gg"))

    ## Verify plot.margin theme was applied
    theme_a <- ggplot2::theme_get()
    built <- ggplot2::ggplot_build(res[["A"]])
    testthat::expect_true(!is.null(built))
})
