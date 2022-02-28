remove_margins <- function(TRKS,
                           dat,
                           verbose=TRUE){
    messager("+ Removing subplot margins...", v=verbose)
    TRKS_std <- list()
    for(x in names(TRKS)){
        # print(x)
        # Some plots only had the labels tranformed, not the actual values.
        ## Check which ones are which and set the limits accordingly.
        TRKS_std[[x]] <-  suppressMessages(suppressWarnings(
            TRKS[[x]] +
                theme(plot.margin = unit(rep(0,4),"cm") )
        ))
    }
    return(TRKS_std)
}