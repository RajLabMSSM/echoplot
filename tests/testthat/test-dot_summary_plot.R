test_that("dot_summary_plot returns ggplot", {

    dat <- echodata::BST1
    gp <- echoplot::dot_summary_plot(dat = dat,
                                     show_plot = FALSE,
                                     verbose = FALSE)
    testthat::expect_true(methods::is(gp, "gg"))
    testthat::expect_true(methods::is(gp, "ggplot"))
})

test_that("dot_summary_plot works with small subset", {

    dat <- echodata::BST1[seq_len(200), ]
    gp <- echoplot::dot_summary_plot(dat = dat,
                                     show_plot = FALSE,
                                     verbose = FALSE)
    testthat::expect_true(methods::is(gp, "gg"))
})
