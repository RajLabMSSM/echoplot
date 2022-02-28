
zoom_polygon <- function(dat,
                         genomic_units="Mb",
                         plot.zoom="5x",
                         alpha=.15,
                         verbose=T){
    messager("+ Constructing zoom polygon...",v=verbose)
    xlims_orig <- get_window_limits(dat = dat,
                                    genomic_units = genomic_units,
                                    verbose = F)
    xlims_zoom <- get_window_limits(dat = dat,
                                    plot.zoom = plot.zoom,
                                    genomic_units = genomic_units,
                                    verbose = F)
    positions <- data.frame(x=c(xlims_zoom, rev(xlims_orig)),
                            y=c(c(1,1),c(0,0)))
    
    zp <- ggplot(positions, aes(x = x, y = y)) +
        geom_polygon(fill="blue",
                     color="transparent",
                     alpha=alpha, show.legend = F) +
        # scale_fill_gradient(limits=c(0.75, 4), low = "lightgrey", high = "red") +
        theme_void() +
        theme(plot.margin = unit(rep(0,4),"cm"),
              axis.text.x = element_blank(),
              axis.title.x = element_blank())
    return(zp)
}
