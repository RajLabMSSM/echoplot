test_that("snp_track_merged works with dataset_type set", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "-log10(P)",
        dataset_type = "eQTL",
        labels_subset = NULL,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged works with label_leadsnp=FALSE", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        label_leadsnp = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged works with custom facet_formula", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        facet_formula = "Method ~ .",
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged works with Mb genomic units and UCS labels", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        genomic_units = "Mb",
        labels_subset = c("Lead", "UCS"),
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged works with remove_duplicates=TRUE", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        remove_duplicates = TRUE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})
