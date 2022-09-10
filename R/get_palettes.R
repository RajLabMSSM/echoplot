#' Get palettes
#' 
#' Get a unique set of palettes, ordered by preference.
#' @param preferred Preferred palette names.
#' @returns Vector of palette names.
#' 
#' @keywords internal
#' @importFrom RColorBrewer brewer.pal.info
get_palettes <- function(preferred=c("Spectral","BrBG","PiYG", "PuOr")){
    unique(
        c(preferred,
          rownames(RColorBrewer::brewer.pal.info))
    )
}
