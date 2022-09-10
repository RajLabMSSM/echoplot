#' Get transcript biotypes
#' 
#' List all available transcript biotypes in the 
#' \link[EnsDb.Hsapiens.v75]{EnsDb.Hsapiens.v75} database. 
#' @param as_filter Return results as an
#'  \link[AnnotationFilter]{TxBiotypeFilter} (default: \code{TRUE}). 
#'  Otherwise, a character vector will be returned.
#' @inheritParams plot_locus
#' @returns Character vector of valid transcript biotypes.
#' 
#' @export
#' @importFrom ensembldb transcripts
#' @importFrom AnnotationFilter TxBiotypeFilter
#' @examples 
#' tx_filter <- get_transcripts_biotypes()
get_tx_biotypes <- function(tx_biotypes=NULL,
                            as_filter=TRUE,
                            verbose=TRUE){ 
    requireNamespace("EnsDb.Hsapiens.v75")
    
    txdb <- EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
    #### Query genome db with finemap_DT coordinates ####
    all_biotypes <- unique(ensembldb::transcripts(txdb)$tx_biotype)
    #### Check whether selected biotypes are valid ####
    if(!is.null(tx_biotypes)){
        tx_biotypes <- all_biotypes[all_biotypes %in% tx_biotypes]
        if(length(tx_biotypes)==0) {
            messager("No matching tx_biotypes found.",v=verbose)
            return(NULL)
        } else {
            messager(length(tx_biotypes),"valid transcript biotype(s) found.",
                     v=verbose)
        }
    } else {
        tx_biotypes <- all_biotypes
    }
    #### Return as an annotation filter ####
    if(isTRUE(as_filter)){
        return(AnnotationFilter::TxBiotypeFilter(value = tx_biotypes))
    } else {
        return(tx_biotypes)
    } 
}
