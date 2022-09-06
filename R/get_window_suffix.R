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
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=1000)
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom=NULL)
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="all")
#' window_suffix <- echoplot:::get_window_suffix(dat=dat, zoom="2x")
#' }
get_window_suffix <- function(dat,
                              zoom,
                              verbose=TRUE){
    messager("+ echoplot:: Get window suffix...",v=verbose)
    window_suffix <- if(is.null(zoom)){
        return(paste0(DescTools::RoundTo(
            (max(dat$POS) - min(dat$POS))/1000, 100),"kb"))
    } else {
        if(is.character(zoom)){
            if(zoom=="all"){
                return("1x") 
            } else {
                return(zoom)
            }
        } else {
            return(paste0(zoom/1000,"kb"))
        }
    }
    return(window_suffix)
}
