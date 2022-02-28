#' Get window size limits for plot
#'
#' @family plot
#' @keywords internal
#' @examples
#' data("BST1");
#' xlims <- get_window_limits(dat=BST1, plot.zoom=50000)
#' xlims <- get_window_limits(dat=BST1, plot.zoom="all")
#' xlims <- get_window_limits(dat=BST1, plot.zoom="5x")
get_window_limits <- function(dat,
                              index_as_center=T,
                              plot.zoom=NULL,
                              genomic_units="Mb",
                              verbose=T){
    # plot.zoom <- c("all","1x","4x",5000);
    # plot.zoom <- c("5000");
    # dat=echolocatoR::BST1;
    # index_as_center=T; genomic_units="Mb"; verbose=T;
    #
    plot.zoom <- if(is.null(plot.zoom)) "1x" else plot.zoom
    plot.zoom[!is.na(plot.zoom)] <- plot.zoom;
    plot.zoom[!is.null(plot.zoom)] <- plot.zoom;
    # Iterate over list of zooms
    xlims_list <- lapply(plot.zoom, function(pz,
                                             .dat=dat,
                                             .index_as_center=index_as_center,
                                             .genomic_units=genomic_units,
                                             .verbose=verbose){
        messager("+ Inferring genomic limits for window:",pz,v=.verbose)
        # Zoom #x as  input
        if(.index_as_center) {
            middle_pos <- subset(.dat, leadSNP)$POS[1]
        } else {
            # Lead Pos isn't always dead middle if manual xlims were used during querying
            middle_pos <- .dat[as.numeric(round(nrow(.dat))/2),]$POS
        }
        
        if(grepl("x$",tolower(pz))){
            if(tolower(pz)=="1x") {
                min_limit <- min(.dat$POS, na.rm = T)
                max_limit <- max(.dat$POS, na.rm = T)
            } else {
                total_bp_span <- (max(.dat$POS, na.rm = T) - min(.dat$POS, na.rm = T))
                new_window <- total_bp_span / as.numeric(gsub("x","",pz))
                # Prevent extending beyond the borders of the data (producing blank space)
                min_limit <- middle_pos - as.integer(new_window/2)
                max_limit <- middle_pos + as.integer(new_window/2)
            }
        } else {
            # Basepairs as input
            if(is.null(pz)){
                min_limit <- min(.dat$POS, na.rm = T)
                max_limit <- max(.dat$POS, na.rm = T)
            } else {
                # 'all' as input
                if(pz=="all"){
                    min_limit <- min(.dat$POS, na.rm = T)
                    max_limit <- max(.dat$POS, na.rm = T)
                } else {
                    min_limit <- middle_pos - as.integer(as.numeric(pz)/2)
                    max_limit <- middle_pos + as.integer(as.numeric(pz)/2)
                }
            }
        }
        xlims <- c(min_limit, max_limit)
        if(.genomic_units=="Mb"){
            xlims <- xlims/1000000
        }
        return(xlims)
    }) %>% `names<-`(plot.zoom)
    
    # For backwards compatibility
    if(length(xlims_list)==1) return(xlims_list[[1]]) else return(xlims_list)
}
