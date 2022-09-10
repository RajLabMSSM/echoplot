test_that("plot_locus works", {
  
    dat<- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(),echodata::locus_dir)

    plt <- echoplot::plot_locus(dat = dat,
                                locus_dir=locus_dir,
                                LD_matrix=LD_matrix, 
                                show_plot = FALSE)
    testthat::expect_true(methods::is(plt$`1x`,"patchwork"))
})
