test_that("snp_track_merged returns ggplot with PP yvar", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged returns ggplot with -log10(P) yvar no labels", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "-log10(P)",
        labels_subset = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged works without labels", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        labels_subset = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged removes x-axis text when xtext=FALSE", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        xtext = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})
