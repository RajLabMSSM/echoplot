#' Plot annotation peaks: XGR
#'
#' Import annotation from \emph{XGR}, filter to only those within the range of
#' \code{dat}, and then plot the peaks.
#' If the annotation has already been downloaded previously, it will be reused.
#'
#' @param dat \link[data.table]{data.table} with at least the following columns:
#' \describe{
#' \item{SNP}{SNP RSID}
#' \item{CHR}{chromosome}
#' \item{POS}{position}
#' }
#' @param n_top Number of top annotations to be plotted
#' (passed to \link[echoannot]{XGR_filter_sources} and then
#' \link[echoannot]{XGR_filter_assays}).
#' @param force_new Download and prepare a new query
#' even if the file already exists locally (Default: \code{FALSE}).
#' @param lib_name Which XGR annotations to check overlap with.
#' For full list of libraries see
#' \href{http://XGR_r-forge.r-project.org/#annotations-at-the-genomic-region-level}{
#'  here.}
#'  Passed to the \code{RData.customised} argument in \link[XGR]{xRDataLoader}.
#' @param locus_dir Locus-specific directory. 
#' @param palette Palette name to be passed to \code{name} argument in 
#' \link[RColorBrewer]{brewer.pal}.
#' @param nThread Number of threads 
#' to parallelise downloading annotations over.
#' @param verbose Print messages.
#' @inheritParams XGR_plot_peaks
#' @inheritParams XGR_prepare_foreground_background
#' @return List with the "data" and the "plot".
#'
#' @export
#' @importFrom echoannot XGR_query annotation_file_name
#' @examples
#' xgr_out <- echoplot::XGR_plot(dat = echodata::BST1[seq_len(1000), ])
XGR_plot <- function(dat,
                     lib_name = "ENCODE_TFBS_ClusteredV3_CellTypes",
                     locus_dir = tempdir(),
                     palette = c("Spectral", "BrBG", "PiYG", "PuOr"),
                     fill_var = "Assay",
                     facet_var = "Source",
                     geom = "density",
                     n_top = 5,
                     adjust = 0.2,
                     force_new = FALSE,
                     show_plot = FALSE,
                     nThread = 1,
                     verbose = TRUE) {
    
    messager("echoannot:: Plotting XGR annotations.", v = verbose)
    #### Create annot file name ####
    annot_file <- echoannot::annotation_file_name(
        locus_dir = locus_dir,
        lib_name = paste0("XGR_", lib_name)
    )
    #### Check if annot file already exists ####
    if (file.exists(annot_file) & force_new == FALSE) {
        gr.lib <- readRDS(annot_file)
    } else {
        gr.lib <- echoannot::XGR_query(
            lib.selections = lib_name,
            dat = dat,
            n_top = n_top,
            nThread = nThread
        )
    } 
    #### Resave filtered annot to save space ####
    saveRDS(gr.lib, annot_file)
    #### Plot annot ####
    xgr_track <- XGR_plot_peaks(
        gr.lib = gr.lib,
        dat = dat,
        adjust = adjust,
        fill_var = fill_var,
        facet_var = facet_var,
        geom = geom,
        show_plot = show_plot
    )
    xgr_track <- XGR_plot_style(
        xgr_track = xgr_track,
        gr.filt = gr.lib,
        lib_name = lib_name,
        palette = palette
    )
    #### Return annot ####
    return(list(
        data = gr.lib,
        plot = xgr_track
    ))
}
