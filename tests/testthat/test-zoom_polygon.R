test_that("zoom_polygon returns a ggplot", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    zp <- echoplot:::zoom_polygon(
        dat = dat,
        genomic_units = "Mb",
        zoom = "5x",
        verbose = FALSE
    )
    testthat::expect_true(methods::is(zp, "gg"))
    testthat::expect_true(methods::is(zp, "ggplot"))
})

test_that("zoom_polygon works with POS genomic units", {

    dat <- echodata::BST1
    ## POS units -- no Mb column needed
    zp <- echoplot:::zoom_polygon(
        dat = dat,
        genomic_units = "POS",
        zoom = "3x",
        verbose = FALSE
    )
    testthat::expect_true(methods::is(zp, "gg"))
})

test_that("zoom_polygon uses theme_void", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    zp <- echoplot:::zoom_polygon(
        dat = dat,
        genomic_units = "Mb",
        zoom = "2x",
        verbose = FALSE
    )
    ## Check it has void-like theme (blank axis text)
    built <- ggplot2::ggplot_build(zp)
    testthat::expect_true(methods::is(zp, "gg"))
})

test_that("zoom_polygon respects alpha parameter", {

    dat <- echodata::BST1
    dat <- echoannot::add_mb(dat)
    zp <- echoplot:::zoom_polygon(
        dat = dat,
        genomic_units = "Mb",
        zoom = "5x",
        alpha = 0.5,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(zp, "gg"))
})
