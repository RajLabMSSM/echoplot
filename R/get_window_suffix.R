#' Get window suffix 
#' 
#' Determine the plot suffix indicating its window size.
#'
#' @family plot
#' @keywords internal
#' @importFrom DescTools RoundTo
#' @source 
#' \code{
#' dat <- echodata::BST1
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, plot_zoom=1000)
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, plot_zoom=NULL)
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, plot_zoom="all")
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, plot_zoom="2x")
#' }
get_window_suffix <- function(dat,
                              plot_zoom,
                              verbose=TRUE){
    messager("+ PLOT:: Get window suffix...",v=verbose)
    window_suffix <- if(is.null(plot_zoom)){
        return(paste0(DescTools::RoundTo(
            (max(dat$POS) - min(dat$POS))/1000, 100),"kb"))
    } else {
        if(is.character(plot_zoom)){
            if(plot_zoom=="all"){
                return("1x") 
            } else {
                return(plot_zoom)
            }
        } else {
            return(paste0(plot_zoom/1000,"kb"))
        }
    }
    return(window_suffix)
}
