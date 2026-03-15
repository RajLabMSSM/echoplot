test_that("snp_track_merged_label adds labels to a base plot", {

    dat <- echodata::BST1[seq_len(200), ]
    ## Build a plot with the required columns via snp_track_merged
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        labels_subset = c("Lead", "CS", "Consensus"),
        show_plot = FALSE,
        verbose = FALSE
    )
    ## The plot data should have shape/size/color columns
    testthat::expect_true(methods::is(plt, "gg"))
    testthat::expect_true(all(c("shape", "size", "color") %in% names(plt$data)))
})

test_that("snp_track_merged_label errors without required columns", {

    ## Create a minimal base plot without shape/size/color
    minimal_dat <- data.frame(
        POS = c(1, 2, 3),
        PP = c(0.1, 0.5, 0.9),
        Method = factor("ABF")
    )
    base <- ggplot2::ggplot(minimal_dat, ggplot2::aes(x = POS, y = PP)) +
        ggplot2::geom_point()
    testthat::expect_error(
        echoplot:::snp_track_merged_label(
            snp_plot = base,
            yvar = "PP",
            genomic_units = "POS",
            verbose = FALSE
        ),
        "All columns must be present"
    )
})

test_that("snp_track_merged_label works with show.legend=FALSE", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        labels_subset = c("Lead"),
        show.legend = FALSE,
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})

test_that("snp_track_merged with Mb genomic_units works", {

    dat <- echodata::BST1[seq_len(200), ]
    plt <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        genomic_units = "Mb",
        show_plot = FALSE,
        verbose = FALSE
    )
    testthat::expect_true(methods::is(plt, "gg"))
})
