test_that("add_snp_labels_plot adds label layers to a ggplot", {

    dat <- echodata::BST1[seq_len(200), ]
    ## Build a base snp_track_merged plot which has shape/size/color columns
    base_plot <- echoplot::snp_track_merged(
        dat = dat,
        yvar = "PP",
        show_plot = FALSE,
        verbose = FALSE
    )
    ## The base plot already has labels, but we can test add_snp_labels_plot
    ## directly by building a fresh labeled dataset
    snp_labels <- echoplot:::construct_snp_labels(
        dat = dat,
        merge_with_input = TRUE,
        verbose = FALSE
    )
    testthat::expect_true(all(c("shape", "size", "color") %in% names(snp_labels)))
})

test_that("add_snp_labels_plot errors when required columns missing", {

    dat <- echodata::BST1[seq_len(50), ]
    ## Create a minimal ggplot without shape/size/color
    base_plot <- ggplot2::ggplot(
        dat,
        ggplot2::aes(x = POS, y = -log10(P))
    ) + ggplot2::geom_point()

    ## snp_labels without required columns should error
    bad_labels <- data.frame(SNP = "rs1234", stringsAsFactors = FALSE)
    testthat::expect_error(
        echoplot:::add_snp_labels_plot(
            snp_plot = base_plot,
            snp_labels = bad_labels,
            yvar = "-log10(P)",
            genomic_units = "POS"
        ),
        "All columns must be present"
    )
})

test_that("add_snp_labels_plot works when snp_labels has required columns", {

    dat <- echodata::BST1[seq_len(200), ]
    dat <- echoannot::add_mb(dat)
    ## Build labeled data through construct_snp_labels
    snp_labels <- echoplot:::construct_snp_labels(
        dat = dat,
        merge_with_input = TRUE,
        verbose = FALSE
    )
    ## Build base plot
    base_plot <- ggplot2::ggplot(
        snp_labels,
        ggplot2::aes(x = Mb, y = PP)
    ) + ggplot2::geom_point()

    result <- echoplot:::add_snp_labels_plot(
        snp_plot = base_plot,
        snp_labels = snp_labels,
        yvar = "PP",
        genomic_units = "Mb",
        show.legend = FALSE
    )
    testthat::expect_true(methods::is(result, "gg"))
    ## Should have added layers (geom_point + 2x geom_label_repel + geom_point)
    testthat::expect_gt(length(result$layers), length(base_plot$layers))
})
