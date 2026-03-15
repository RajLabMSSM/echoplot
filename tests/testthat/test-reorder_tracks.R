test_that("reorder_tracks uses default order from heights_dict", {

    gg <- ggplot2::ggplot()
    TRKS <- list("Fine-mapping" = gg, "GWAS" = gg, "Genes" = gg)
    res <- echoplot:::reorder_tracks(TRKS = TRKS, verbose = FALSE)
    ## Default order from heights_dict puts Genes before GWAS
    default_order <- names(echoplot:::heights_dict())
    actual_order <- names(res)
    ## Genes comes before GWAS in the default dict
    testthat::expect_true(
        which(actual_order == "Genes") < which(actual_order == "GWAS")
    )
    testthat::expect_length(res, 3)
})

test_that("reorder_tracks respects custom track_order", {

    gg <- ggplot2::ggplot()
    TRKS <- list("A" = gg, "B" = gg, "C" = gg)
    res <- echoplot:::reorder_tracks(
        TRKS = TRKS,
        track_order = c("C", "A", "B"),
        verbose = FALSE
    )
    testthat::expect_equal(names(res), c("C", "A", "B"))
})

test_that("reorder_tracks keeps tracks not in track_order", {

    gg <- ggplot2::ggplot()
    TRKS <- list("A" = gg, "B" = gg, "C" = gg)
    res <- echoplot:::reorder_tracks(
        TRKS = TRKS,
        track_order = c("B"),
        verbose = FALSE
    )
    ## B goes first, then A and C appended
    testthat::expect_equal(names(res)[1], "B")
    testthat::expect_length(res, 3)
})
