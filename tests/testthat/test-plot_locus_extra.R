test_that("plot_locus works with labels_subset=NULL", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        labels_subset = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})

test_that("plot_locus works with plot_full_window=FALSE", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        plot_full_window = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})

test_that("plot_locus works with zoom parameter", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        zoom = c("1x", "4x"),
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_length(plt, 2)
    testthat::expect_true("1x" %in% names(plt))
    testthat::expect_true("4x" %in% names(plt))
})

test_that("plot_locus works with return_list=TRUE", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        return_list = TRUE,
        show_plot = FALSE,
        verbose = FALSE
    )
    ## return_list=TRUE returns individual track list rather than patchwork
    testthat::expect_true(is.list(plt$`1x`))
    testthat::expect_true(all(
        vapply(plt$`1x`, function(x) methods::is(x, "gg"), logical(1))
    ))
})

test_that("plot_locus works without LD_matrix", {

    dat <- echodata::BST1
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})

test_that("plot_locus works with gene_track=FALSE", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        gene_track = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})

test_that("plot_locus works with snp_group_lines=NULL", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        snp_group_lines = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})

test_that("plot_locus works with save_RDS=TRUE", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir, "test_save")
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        save_RDS = TRUE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
    ## Check that plot_tracks directory was created
    testthat::expect_true(dir.exists(file.path(locus_dir, "plot_tracks")))
    ## Clean up
    unlink(locus_dir, recursive = TRUE)
})

test_that("plot_locus works with mean.PP=FALSE", {

    dat <- echodata::BST1
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path(tempdir(), echodata::locus_dir)
    plt <- echoplot::plot_locus(
        dat = dat,
        locus_dir = locus_dir,
        LD_matrix = LD_matrix,
        mean.PP = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt$`1x`, "patchwork"))
})
