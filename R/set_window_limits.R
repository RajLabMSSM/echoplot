
set_window_limits <- function(TRKS,
                              dat,
                              plot.zoom,
                              exceptions_str="*full window$|zoom_polygon",
                              verbose=T){
    messager("+ Aligning xlimits for each subplot...",v=verbose)
    ### Exclude the purposefully unzoomed tracks
    exceptions <- grep(exceptions_str, names(TRKS), value=T)
    
    for(x in names(TRKS)){
        # print(x)
        trk <- TRKS[[x]]
        genomic_units <- guess_genomic_units(gg = trk)
        xlims <- get_window_limits(dat=dat,
                                   plot.zoom = if(x %in% exceptions) "1x" else plot.zoom,
                                   genomic_units=genomic_units,
                                   verbose=F)
        unit_divisor <- if(genomic_units=="Mb") 1 else 1000000
        
        #### IMPORTANT!####
        #  Setting x-limits via coord_cartesian() ensures data that
        ## extends outside these limits will still be displayed.
        ## In contrast xlim() will simply throw out this data.
        # https://stackoverflow.com/questions/25685185/limit-ggplot2-axes-without-removing-data-outside-limits-zoom
        suppressMessages(suppressWarnings(
            TRKS[[x]] <- trk +
                scale_x_continuous(labels = function(x)x/unit_divisor) +
                # limits = xlims) +
                # xlim(xlims)
                coord_cartesian(xlim=c(xlims))
        ))
    }
    return(TRKS)
}

