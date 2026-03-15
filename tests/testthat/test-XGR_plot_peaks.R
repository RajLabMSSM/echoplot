test_that("XGR_plot_peaks returns a ggplot", {

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
    testthat::expect_true(methods::is(trk, "gg"))
})

test_that("XGR_plot_peaks works with histogram geom", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        geom = "histogram",
        show_plot = FALSE
    )
    testthat::expect_true(methods::is(trk, "gg"))
})

test_that("XGR_plot_peaks works with trim_xlims=TRUE", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        trim_xlims = TRUE,
        show_plot = FALSE
    )
    testthat::expect_true(methods::is(trk, "gg"))
})

test_that("XGR_plot_peaks works with as_ggplot=FALSE", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        as_ggplot = FALSE,
        show_plot = FALSE
    )
    ## When as_ggplot=FALSE, should return a ggbio object (GGbio)
    testthat::expect_false(methods::is(trk, "ggplot"))
})

test_that("XGR_plot_peaks respects adjust parameter", {

    gr.lib <- echoannot::xgr_example
    gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
    gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
    trk <- echoplot::XGR_plot_peaks(
        gr.lib = gr.filt,
        dat = echodata::BST1,
        fill_var = "Assay",
        facet_var = "Source",
        adjust = 0.5,
        show_plot = FALSE
    )
    testthat::expect_true(methods::is(trk, "gg"))
})
