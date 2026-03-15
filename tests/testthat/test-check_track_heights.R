test_that("check_track_heights uses heights_dict when no heights given", {

    ## Create minimal ggplot objects to act as tracks
    gg <- ggplot2::ggplot()
    TRKS <- list("GWAS" = gg, "Fine-mapping" = gg)
    res <- echoplot:::check_track_heights(TRKS = TRKS, verbose = FALSE)
    testthat::expect_length(res, 2)
    testthat::expect_true(is.numeric(res))
})

test_that("check_track_heights truncates user heights to match tracks", {

    gg <- ggplot2::ggplot()
    TRKS <- list("A" = gg, "B" = gg)
    res <- echoplot:::check_track_heights(
        TRKS = TRKS,
        track_heights = c(1, 2, 3, 4),
        verbose = FALSE
    )
    testthat::expect_length(res, 2)
})

test_that("check_track_heights fills missing heights with default", {

    gg <- ggplot2::ggplot()
    TRKS <- list("A" = gg, "B" = gg, "C" = gg)
    res <- echoplot:::check_track_heights(
        TRKS = TRKS,
        track_heights = c(0.5),
        default_height = 2,
        verbose = FALSE
    )
    ## 1 user-provided + 2 filled in
    testthat::expect_length(res, 3)
    testthat::expect_equal(res[2], 2)
    testthat::expect_equal(res[3], 2)
})
