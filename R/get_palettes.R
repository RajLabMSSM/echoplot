#' Get palettes
#' 
#' Get a unique set of palettes from \pkg{pals}, ordered by preference.
#' @param preferred Preferred palette names. 
#' Will be moved to the top of the list.
#' @param n_pals Max number of palettes to return.
#' @param n Max number of colors to return per palette.
#'  Only applicable when \code{names_only=FALSE}. 
#' @param names_only Only return the names of the palettes 
#' (\code{TRUE}), rather than a named list of colors for each palette
#'  (default: \code{TRUE}).
#' @returns Vector of palette names.
#' 
#' @export
#' @importFrom utils getFromNamespace
#' @importFrom stats setNames
#' @examples 
#' palettes <- get_palettes()
get_palettes <- function(preferred = c("brewer.spectral", 
                                       "brewer.brbg",
                                       "brewer.piyg", 
                                       "brewer.puor"),
                         n_pals=NULL,
                         n=8,
                         names_only=FALSE){
    
    #### Filter all palettes ####
    all_pals <- names(utils::getFromNamespace(x = "syspals",
                                              ns = "pals"))
    ns <- list_namespace(packages = "pals", 
                         status_filter = "export",
                         verbose = FALSE)
    all_pals <- all_pals[all_pals %in% ns$fun]
    #### Filter preferred ####
    preferred <- preferred[preferred %in% all_pals]
    palettes <- unique(c(preferred,all_pals))
    #### Limit palette number ####
    if(!is.null(n_pals)){
        palettes <- palettes[seq_len(n_pals)]
    }
    if(isTRUE(names_only)){
        return(palettes)
    } else {
        colors <- lapply(stats::setNames(palettes,palettes),
                         function(p){
            pals_func <- utils::getFromNamespace(x = p, 
                                                 ns = "pals")
            pals_func(n = n)
        })
        return(colors)
    } 
}
