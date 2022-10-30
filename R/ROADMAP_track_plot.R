#' Plot ROADMAP query
#'
#' @param grl.roadmap ROADMAP query results.
#' @param gr.snp Optionally, can include an extra \link[GenomicRanges]{GRanges}
#'  object to ensure the plot does not extend beyond certain coordinates.
#' @param geom The type of plot to create.
#' Options include "density" and "histogram".
#' @param adjust The granularity of the peaks.
#' @param show_plot Whether to print the plot.
#' @source
#' \code{
#' gr.snp <- echodata::dt_to_granges(echodata::BST1)
#' grl.roadmap <- echoannot::ROADMAP_query(
#'     query_dat = gr.snp,
#'     merge_and_process = TRUE,
#'     keyword_query = "monocyte")
#' track.roadmap <- ROADMAP_track_plot(grl.roadmap,
#'     gr.snp = gr.snp)
#' }
#' @keywords internal
#' @importFrom ggbio autoplot
#' @importFrom ggplot2 aes theme_classic theme  element_text
#' @importFrom ggplot2 guides guide_legend scale_y_continuous
#' @importFrom methods show
ROADMAP_track_plot <- function(grl.roadmap,
                               gr.snp = NULL,
                               geom = "density",
                               adjust = 0.2,
                               position = c("stack","fill","dodge"),
                               show_plot = TRUE,
                               as_ggplot = TRUE,
                               verbose = TRUE) {
    
    messager("Generating ROADMAP track plot.", v = verbose)
    track.roadmap <- ggbio::autoplot(
        object = grl.roadmap,
        # which = gr.snp,
        ggplot2::aes_string(fill = "chrom_state"),
        color = "white",
        size = .1,
        geom = geom,
        adjust = adjust,
        # bins=10,
        position = position[[1]],
        facets = Source ~ .,
        alpha = 1
    ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            strip.text.y = ggplot2::element_text(angle = 0),
            strip.text = ggplot2::element_text(size = 9)
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(
            ncol = 2,
            keyheight = .5,
            keywidth = .5
        )) +
        ggplot2::scale_y_continuous(n.breaks = 3)
    if (show_plot) {
        methods::show(track.roadmap)
    }
    if (as_ggplot) {
        return(track.roadmap@ggplot)
    } else {
        return(track.roadmap)
    }
}
