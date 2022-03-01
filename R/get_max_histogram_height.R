#' Get max histogram height
#' 
#' Get the max height in a histogram plot.
#' @keywords internal
#' @importFrom DescTools RoundTo
get_max_histogram_height <- function(gg,
                                     round_to=NULL,
                                     verbose=TRUE){
    requireNamespace("ggplot2")
    
    if(echodata::is_ggbio(gg)) gg <- gg@ggplot
    messager("+ PLOT:: Calculating max histogram height",v=verbose)
    dat <- ggplot2::ggplot_build(gg)$data[[1]]
    max_height <- max(dat$ymax, na.rm = TRUE)
    if(!is.null(round_to)){
        max_height <- DescTools::RoundTo(max_height, round_to)
    }
    return(max_height)
}
