guess_genomic_units <- function(gg,
                                decimals_default="Mb",
                                verbose=FALSE){
    
    gp <- ggplot2::ggplot_build(gg)
    data_dims <- lapply(gp$data, nrow) |> unlist()
    i <- which(data_dims==max(data_dims))[1]
    plot_dat <- gp$data[[i]]
    x_var <- c("xend","x")[c("xend","x") %in% colnames(plot_dat)][1]
    ### Guess based on  the presence of decimals (which implies non-POS units)
    ### Currently cannot distinguish between different non-POS units (Gb,Mb,Kb) 
    ## so we just assume Mb.
    # max(plot_dat[[x_var]], na.rm = T)%%1==0 ### Doesn't actually work
    max_val <- max(plot_dat[[x_var]], na.rm = TRUE)
    
    genomic_units <- if(isTRUE(all.equal(round(max_val,1), max_val))){
        "POS"
    } else {
        decimals_default
    }
    messager("+ Guessing genomic units:",genomic_units,v=verbose)
    return(genomic_units)
}
