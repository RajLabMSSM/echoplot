#' Plot annotation peaks: ROADMAP
#'
#' Import annotation from \emph{ROADMAP}, filter to only those within the range
#' of \code{dat}, and then plot the peaks.
#' If the annotation has already been downloaded previously, it will be reused.
#'
#' @param n_top Number of top annotations to be plotted
#' (passed to \link[echoannot]{ROADMAP_query}). 
#' @param lib_name Name of the data library to use.
#' @param locus_dir Locus-specific directory.
#' @param roadmap_query Search all columns in the Roadmap annotations metadata
#' and only query annotations that contain your keywords.
#' Can provide multiple keywords in list form:
#' \code{c("placenta","liver","monocytes")}
#'
#' @inheritParams XGR_plot
#' @inheritParams ROADMAP_track_plot
#' @inheritParams echoannot::ROADMAP_query
#' @inheritParams echoannot::ROADMAP_construct_reference
#' @inheritParams echoannot::ROADMAP_tabix
#' 
#' @returns A named list containing:
#' \itemize{
#' \item{"data"}{\code{GRanges} object within the queried coordinates.}
#' \item{"plot"}{\code{ggbio} plot.}
#' }
#'
#' @export
#' @importFrom echodata dt_to_granges
#' @importFrom echoannot annotation_file_name ROADMAP_query
#' @examples
#' dat <- echodata::BST1[seq_len(1000),]
#' roadmap_out <- echoplot::ROADMAP_plot(
#'     dat = dat,
#'     roadmap_query = "monocyte")
ROADMAP_plot <- function(dat,
                         roadmap_query,
                         lib_name = "Roadmap.ChromatinMarks_CellTypes",
                         locus_dir = tempdir(),
                         limit_files = NULL,
                         n_top = 5,
                         adjust = 0.2,
                         force_new = FALSE,
                         show_plot = FALSE, 
                         conda_env = "echoR_mini",
                         nThread = 1,
                         verbose = TRUE) {
    
    # echoverseTemplate:::args2vars(ROADMAP_plot)
    # echoverseTemplate:::source_all()
    
    messager("echoannot:: Plotting ROADMAP annotations.", v = verbose)
    gr.snp <- echodata::dt_to_granges(dat = dat,
                                      style = "UCSC",
                                      verbose = verbose) 
    #### Download (or import existing) ####
    grl.roadmap <- echoannot::ROADMAP_query( 
        # Will convert data.table automatically
        query_dat = dat,
        keyword_query = roadmap_query,
        merge_and_process = TRUE,
        limit_files = limit_files,
        n_top = n_top,
        conda_env = conda_env,
        nThread = nThread,
        verbose = verbose)
    #### Plot ####
    track.roadmap <- ROADMAP_track_plot(
        grl.roadmap = grl.roadmap,
        gr.snp = gr.snp,
        adjust = adjust,
        show_plot = show_plot,
        verbose = verbose
    )
    #### Return ####
    return(list(
        data = grl.roadmap,
        plot = track.roadmap
    ))
}
