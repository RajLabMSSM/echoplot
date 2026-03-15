test_that("plot_locus_multi generates default names when conditions=NULL", {

    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::filter_snps(echodata::BST1, bp_distance = 10000)
    dat_ls <- list(dat, dat)
    ## No names -- conditions=NULL triggers default naming
    LD_matrix <- echodata::BST1_LD_matrix
    LD_ls <- list(ancestry1 = LD_matrix, ancestry2 = LD_matrix)
    plot_list <- echoplot::plot_locus_multi(
        dat_ls = dat_ls,
        LD_ls = LD_ls,
        locus_dir = locus_dir,
        conditions = NULL,
        show_plot = FALSE,
        return_list = TRUE,
        verbose = FALSE
    )
    testthat::expect_true(is.list(plot_list))
    testthat::expect_true("1x" %in% names(plot_list))
})

test_that("plot_locus_multi with explicit conditions matching dat_ls", {

    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::filter_snps(echodata::BST1, bp_distance = 10000)
    dat_ls <- list(gwas1 = dat, gwas2 = dat)
    LD_matrix <- echodata::BST1_LD_matrix
    LD_ls <- list(ancestry1 = LD_matrix, ancestry2 = LD_matrix)
    plot_list <- echoplot::plot_locus_multi(
        dat_ls = dat_ls,
        LD_ls = LD_ls,
        locus_dir = locus_dir,
        conditions = c("EUR", "AFR"),
        show_plot = FALSE,
        return_list = TRUE,
        verbose = FALSE
    )
    testthat::expect_true(is.list(plot_list))
    testthat::expect_true("1x" %in% names(plot_list))
})

test_that("plot_locus_multi returns merged patchwork by default", {

    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    dat <- echodata::filter_snps(echodata::BST1, bp_distance = 10000)
    dat_ls <- list(gwas1 = dat, gwas2 = dat)
    LD_matrix <- echodata::BST1_LD_matrix
    LD_ls <- list(ancestry1 = LD_matrix, ancestry2 = LD_matrix)
    plot_list <- echoplot::plot_locus_multi(
        dat_ls = dat_ls,
        LD_ls = LD_ls,
        locus_dir = locus_dir,
        show_plot = FALSE,
        return_list = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(is.list(plot_list))
    testthat::expect_true(methods::is(plot_list$`1x`, "patchwork"))
})
