#' Determine the plot suffix indicating its window size
#'
#' @family plot
#' @keywords internal
#' @examples
#' data("BST1")
#' window_suffix <- get_window_suffix(dat=BST1, plot.zoom=1000)
#' window_suffix <- get_window_suffix(dat=BST1, plot.zoom=NULL)
#' window_suffix <- get_window_suffix(dat=BST1, plot.zoom="all")
#' window_suffix <- get_window_suffix(dat=BST1, plot.zoom="2x")
get_window_suffix <- function(dat,
                              plot.zoom,
                              verbose=T){
    messager("+ PLOT:: Get window suffix...",v=verbose)
    window_suffix <- if(is.null(plot.zoom)){
        return(paste0(DescTools::RoundTo((max(dat$POS) - min(dat$POS))/1000, 100),"kb"))
    } else {
        if(is.character(plot.zoom)){
            if(plot.zoom=="all"){
                return("1x")
                # return(paste0(DescTools::RoundTo((max(dat$POS) - min(dat$POS))/1000, 100),"kb"))
            } else {
                return(plot.zoom)
            }
        } else {
            return(paste0(plot.zoom/1000,"kb"))
        }
    }
}


