merge_datasets <- function(dat_ls, 
                           # suffixes=paste0(".",names(dat_ls)),
                           required_cols=c("CHR","POS","SNP","P"),
                           suffixes=paste0(".",seq_len(length(dat_ls))),
                           verbose=TRUE){  
    
    messager("Merging list of primary datasets.",v=verbose)
    dat_merged <- lapply(names(dat_ls), 
         function(nm){
             d <- dat_ls[[nm]]
             missing_cols <- required_cols[!required_cols %in% names(d)]
             if(length(missing_cols)>0){
                 stp <- paste("Dataset",paste0("'",nm,"'"),
                              "is missing the following required column(s):",
                              paste0("\n - ",missing_cols,collapse = ""))
                 stop(stp)
             }
             return(d)
             # d[,c("CHR","POS","SNP","ZSCORE")]
         }) |> Reduce(f = function(...){
             merge(..., 
                   by=c("CHR","POS","SNP"),
                   suffixes=suffixes)
         })
    return(dat_merged)
}
