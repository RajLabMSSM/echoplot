
zoom_highlight <- function(gg,
                           dat,
                           plot.zoom="5x",
                           alpha=.15,
                           verbose=T){
    messager("+ Highlighting zoom origin...",v=verbose)
    genomic_units <- guess_genomic_units(gg = gg)
    xlims_zoom <- get_window_limits(dat = dat,
                                    plot.zoom = plot.zoom,
                                    genomic_units = genomic_units,
                                    verbose = F)
    rect_dat <- data.frame(x=c(xlims_zoom, rev(xlims_zoom)), y=c(-Inf,-Inf,Inf,Inf))
    gg2 <- gg +
        ggplot2::geom_polygon(data = rect_dat,
                              aes(x=x, y=y),
                              color="transparent",
                              alpha=alpha,
                              fill="blue")
    return(gg2)
}
