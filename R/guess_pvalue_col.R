guess_pvalue_col <- function(dat,
                             qtl_suffix=NULL){
    requireNamespace("stringr")
    
    p_options <- c("p","p-value","p-values","pvalue","pvalues","pval")
    p_options <- c(p_options, stringr::str_to_sentence(p_options))
    pval_col <- paste0(p_options,qtl_suffix)[
        paste0(p_options,qtl_suffix) %in% colnames(dat)
    ] 
    return(pval_col)
}
