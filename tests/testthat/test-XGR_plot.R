test_that("XGR_plot works", {
    
    dat <- echodata::BST1[seq_len(1000),]
    xgr_out <- echoplot::XGR_plot(dat = dat,
                                  force_new = TRUE,
                                  n_top = 3)
    testthat::expect_length(xgr_out, 2)
    testthat::expect_true(methods::is(xgr_out$data, "GRanges"))
    testthat::expect_true(methods::is(xgr_out$plot, "gg"))
    testthat::expect_length(xgr_out$data, 32)
})
