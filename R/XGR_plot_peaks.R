#' Plot XGR peaks
#'
#' Plots the distribution of annotations across a genomic region (x-axis).
#'
#' @param fill_var Fill variable.
#' @param facet_var Row facet variable.
#' @param geom Plot type ("density" or "histogram").
#' @param gr.lib \code{GRanges} object of annotations.
#' @param geom Plot type ("density", or "histogram").
#' @param locus [Optional] Locus name.
#' @param adjust The granularity of the peaks.
#' @param show_plot Print the plot.
#' @param trim_xlims Trim the x-axis limits.
#' @return \code{ggbio} track plot.
#' @inheritParams echoannot::XGR_prepare_foreground_background
#' @inheritParams echoannot::NOTT2019_epigenomic_histograms
#' @inheritParams ggbio::autoplot
#'
#' @family XGR
#' @export
#' @importFrom GenomicRanges mcols
#' @importFrom methods show
#' @examples
#' #### Import example query ####
#' gr.lib <- echoannot::xgr_example
#'
#' #### Filter query ####
#' gr.filt <- echoannot::XGR_filter_sources(gr.lib = gr.lib)
#' gr.filt <- echoannot::XGR_filter_assays(gr.lib = gr.filt)
#'
#' #### Plot query #####
#' XGR_track <- echoplot::XGR_plot_peaks(
#'     gr.lib = gr.filt,
#'     dat = echodata::BST1,
#'     fill_var = "Assay",
#'     facet_var = "Source")
XGR_plot_peaks <- function(gr.lib,
                           dat,
                           fill_var = "Assay",
                           facet_var = "Source",
                           geom = "density",
                           locus = NULL,
                           adjust = .2,
                           show_plot = TRUE,
                           legend = TRUE,
                           as_ggplot = TRUE,
                           trim_xlims = FALSE) {
    # data("BST1"); dat <- BST1; show.legend=T;
    # fill_var="Assay"; facet_var="Source"; geom="density"; adjust=.2;
    
    requireNamespace("ggplot2")
    gr.lib$facet_label <- gsub(
        "_", "\n",
        GenomicRanges::mcols(gr.lib)[, facet_var]
    )
    XGR_track <- ggbio::autoplot(gr.lib,
        # which = gr.snp,
        ggplot2::aes(fill = eval(parse(text = fill_var))),
        # formula(paste0(facet_var," ~ .")),
        facets = stats::formula("facet_label ~ ."),
        # fill = "magenta",
        color = "white", # NA
        geom = geom,
        adjust = adjust,
        position = "stack",
        # bins=50,
        size = .1,
        alpha = .7,
        legend = legend
    ) +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = fill_var)
    if (trim_xlims) {
        XGR_track <- suppressMessages(
            XGR_track +
                ggplot2::xlim(min(dat$POS), max(dat$POS))
        )
    }
    # ggbio::tracks(list("XGR"=XGR_track))
    if (show_plot) methods::show(XGR_track)
    if (as_ggplot) {
        return(XGR_track@ggplot)
    } else {
        return(XGR_track)
    }
}
