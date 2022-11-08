set_window_limits <- function(TRKS,
                              dat,
                              zoom,
                              exceptions_str="*full window$|zoom_polygon",
                              verbose=TRUE){
    requireNamespace("ggplot2")
    messager("+ Aligning xlimits for each subplot...",v=verbose)
    ### Exclude the purposefully unzoomed tracks
    exceptions <- grep(exceptions_str, names(TRKS), value=TRUE)
    
    for(x in names(TRKS)){ 
        trk <- TRKS[[x]]
        genomic_units <- guess_genomic_units(gg = trk)
        xlims <- echoannot::get_window_limits(
            dat=dat,
            zoom = if(x %in% exceptions) "1x" else zoom,
            genomic_units=genomic_units,
            verbose=FALSE)
        unit_divisor <- if(genomic_units=="Mb") 1 else 1000000
        
        #### IMPORTANT!####
        #  Setting x-limits via coord_cartesian() ensures data that
        ## extends outside these limits will still be displayed.
        ## In contrast xlim() will simply throw out this data.
        # https://stackoverflow.com/questions/25685185/limit-ggplot2-axes-without-removing-data-outside-limits-zoom
        suppressMessages(suppressWarnings(
            TRKS[[x]] <- trk +
                ggplot2::scale_x_continuous(
                    labels = function(x)x/unit_divisor) +
                # limits = xlims) +
                # xlim(xlims)
                ggplot2::coord_cartesian(xlim=c(xlims))
        ))
    }
    return(TRKS)
}

