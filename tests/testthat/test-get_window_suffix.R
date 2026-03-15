test_that("get_window_suffix with zoom=NULL computes from data range", {

    dat <- echodata::BST1
    res <- echoplot:::get_window_suffix(dat = dat, zoom = NULL, verbose = FALSE)
    testthat::expect_true(is.character(res))
    testthat::expect_true(grepl("kb$", res))
})

test_that("get_window_suffix with zoom='all' returns '1x'", {

    dat <- echodata::BST1
    res <- echoplot:::get_window_suffix(
        dat = dat, zoom = "all", verbose = FALSE
    )
    testthat::expect_equal(res, "1x")
})

test_that("get_window_suffix with character zoom returns it directly", {

    dat <- echodata::BST1
    res <- echoplot:::get_window_suffix(
        dat = dat, zoom = "5x", verbose = FALSE
    )
    testthat::expect_equal(res, "5x")
})

test_that("get_window_suffix with numeric zoom converts to kb", {

    dat <- echodata::BST1
    res <- echoplot:::get_window_suffix(
        dat = dat, zoom = 50000, verbose = FALSE
    )
    testthat::expect_equal(res, "50kb")
})
