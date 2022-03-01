
zoom_polygon <- function(dat,
                         genomic_units="Mb",
                         plot_zoom="5x",
                         alpha=.15,
                         verbose=TRUE){
    requireNamespace("ggplot2")
    x <- y <- NULL;
    
    messager("+ Constructing zoom polygon...",v=verbose)
    xlims_orig <- get_window_limits(dat = dat,
                                    genomic_units = genomic_units,
                                    verbose = FALSE)
    xlims_zoom <- get_window_limits(dat = dat,
                                    plot_zoom = plot_zoom,
                                    genomic_units = genomic_units,
                                    verbose = FALSE)
    positions <- data.frame(x=c(xlims_zoom, rev(xlims_orig)),
                            y=c(c(1,1),c(0,0)))
    
    zp <- ggplot2::ggplot(positions, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_polygon(fill="blue",
                     color="transparent",
                     alpha=alpha, show.legend = FALSE) +
        # scale_fill_gradient(limits=c(0.75, 4), low = "lightgrey", high = "red") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::unit(rep(0,4),"cm"),
              axis.text.x = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank())
    return(zp)
}
