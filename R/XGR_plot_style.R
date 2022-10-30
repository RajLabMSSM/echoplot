XGR_plot_style <- function(xgr_track,
                           gr.filt,
                           lib_name,
                           palette = c("Spectral", "BrBG", "PiYG", "PuOr")) {
    requireNamespace("ggplot2")
    #### further modify ####
    colourCount <- length(unique(gr.filt$Assay))
    trk_name <- paste("XGR:", lib_name)
    xgr_track <- suppressMessages(
        xgr_track +
            ggplot2::theme_classic() +
            ggplot2::theme(
                strip.text.y = ggplot2::element_text(angle = 0),
                strip.text = ggplot2::element_text(size = 9)
            ) +
            ggplot2::scale_fill_manual(
                values = grDevices::colorRampPalette(
                    RColorBrewer::brewer.pal(8, name = palette[1])
                )(colourCount)
            ) +
            ggplot2::scale_y_continuous(n.breaks = 3) +
            ggplot2::guides(fill = ggplot2::guide_legend(
                ncol = 2,
                keyheight = .5,
                keywidth = .5
            )) +
            ggplot2::labs(y = paste(gsub("_", " ", trk_name), 
                                    "\n", "density"))
    )
    return(xgr_track)
}
