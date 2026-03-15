test_that("XGR_plot_style returns a styled ggplot", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    ## Build base track first
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        show_plot = FALSE
    )
    n_assays <- length(unique(gr.filt$Assay))
    colors <- echoplot::get_palettes(
        n_pals = 1,
        n = max(n_assays, 3)
    )[[1]][seq_len(n_assays)]
    styled <- echoplot:::XGR_plot_style(
        xgr_track = trk,
        gr.filt = gr.filt,
        lib_name = "ENCODE_TFBS_ClusteredV3_CellTypes",
        colors = colors
    )
    testthat::expect_true(methods::is(styled, "gg"))
})

test_that("XGR_plot_style sets y-axis label based on lib_name", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        show_plot = FALSE
    )
    n_assays <- length(unique(gr.filt$Assay))
    colors <- echoplot::get_palettes(
        n_pals = 1,
        n = max(n_assays, 3)
    )[[1]][seq_len(n_assays)]
    lib_name <- "Broad_Histone"
    styled <- echoplot:::XGR_plot_style(
        xgr_track = trk,
        gr.filt = gr.filt,
        lib_name = lib_name,
        colors = colors
    )
    testthat::expect_true(methods::is(styled, "gg"))
    ## y-label should contain the lib_name (underscores replaced)
    y_lab <- styled$labels$y
    testthat::expect_true(grepl("Broad Histone", y_lab))
})
