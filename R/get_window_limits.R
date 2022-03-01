#' Get window limits
#' 
#' Get window size limits for Manhattan plot.
#' @family plot
#' @keywords internal
#' @examples
#' dat <- echodata::BST1
#' xlims <- get_window_limits(dat=dat, plot_zoom=50000)
#' xlims <- get_window_limits(dat=dat, plot_zoom="all")
#' xlims <- get_window_limits(dat=dat, plot_zoom="5x")
get_window_limits <- function(dat,
                              index_as_center=TRUE,
                              plot_zoom=NULL,
                              genomic_units="Mb",
                              verbose=TRUE){
    # plot_zoom <- c("all","1x","4x",5000);
    # plot_zoom <- c("5000");
    # dat=echolocatoR::BST1;
    # index_as_center=T; genomic_units="Mb"; verbose=T;
    leadSNP <- NULL;
    
    plot_zoom <- if(is.null(plot_zoom)) "1x" else plot_zoom
    plot_zoom[!is.na(plot_zoom)] <- plot_zoom;
    plot_zoom[!is.null(plot_zoom)] <- plot_zoom;
    # Iterate over list of zooms
    xlims_list <- lapply(plot_zoom, function(pz,
                                             .dat=dat,
                                             .index_as_center=index_as_center,
                                             .genomic_units=genomic_units,
                                             .verbose=verbose){
        leadSNP <- NULL;
        messager("+ Inferring genomic limits for window:",pz,v=.verbose)
        # Zoom #x as  input
        if(.index_as_center) {
            middle_pos <- subset(.dat, leadSNP)$POS[1]
        } else {
            # Lead Pos isn't always dead middle if 
            # manual xlims were used during querying
            middle_pos <- .dat[as.numeric(round(nrow(.dat))/2),]$POS
        }
        
        if(grepl("x$",tolower(pz))){
            if(tolower(pz)=="1x") {
                min_limit <- min(.dat$POS, na.rm = TRUE)
                max_limit <- max(.dat$POS, na.rm = TRUE)
            } else {
                total_bp_span <- (max(.dat$POS, na.rm = TRUE) - 
                                      min(.dat$POS, na.rm = TRUE))
                new_window <- total_bp_span / as.numeric(gsub("x","",pz))
                # Prevent extending beyond the borders of the data 
                # (producing blank space)
                min_limit <- middle_pos - as.integer(new_window/2)
                max_limit <- middle_pos + as.integer(new_window/2)
            }
        } else {
            # Basepairs as input
            if(is.null(pz)){
                min_limit <- min(.dat$POS, na.rm = TRUE)
                max_limit <- max(.dat$POS, na.rm = TRUE)
            } else {
                # 'all' as input
                if(pz=="all"){
                    min_limit <- min(.dat$POS, na.rm = TRUE)
                    max_limit <- max(.dat$POS, na.rm = TRUE)
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
    }) %>% `names<-`(plot_zoom)
    
    # For backwards compatibility
    if(length(xlims_list)==1) return(xlims_list[[1]]) else return(xlims_list)
}
