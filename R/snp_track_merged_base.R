snp_track_merged_base <- function(finemap_melt,
                                  genomic_units,
                                  yvar,
                                  point_alpha,
                                  show.legend,
                                  sig_cutoff,
                                  cutoff_lab,
                                  facet_formula,
                                  strip.text.y.angle){
    snp_plot <- ggplot2::ggplot(
        data = finemap_melt,
        ggplot2::aes_string(
            x=genomic_units, y=yvar, 
            color=if("r2" %in% names(finemap_melt)) "r2" else NULL
        )) +
        # Bottom plot delineator
        ggplot2::geom_hline(yintercept=0) +
        ggplot2::geom_point(alpha=point_alpha, 
                            show.legend = show.legend,
                            na.rm = TRUE) +
        ggplot2::scale_color_gradient(low="blue",high ="red",
                                      breaks=c(0,.5,1), limits=c(0,1)) +
        ## Sig cutoff line
        ggplot2::geom_hline(yintercept = sig_cutoff, 
                            alpha=.5, linetype=2, size=.5, 
                            color="black") +
        ggplot2::geom_text(
            data = finemap_melt[1,],
            ggplot2::aes_string(x=paste0("(",genomic_units,")"), 
                                y="sig_cutoff*1.1"),
            label=cutoff_lab,
            size=3, color="grey", hjust = 2) +
        ggplot2::labs(color=bquote(r^2),
                      y=if(startsWith(yvar,"-log10")){
                          bquote("-log"[10]~"(p)")
                      } else {yvar} ) +
        ggplot2::theme_classic() +
        ggplot2::facet_grid(facets =if(is.null(facet_formula)) {
            facet_formula
        } else  {stats::as.formula(facet_formula)}) +
        ggplot2::theme(
            strip.text.y = ggplot2::element_text(angle=strip.text.y.angle)
        ) +
        ggplot2::guides(
            fill = ggplot2::guide_colourbar(barwidth = 1,
                                            barheight = 3))
    ###  Choose breaks and ylims ####
    if(startsWith(yvar,"-log")){
        pval_stripped <- gsub("-log|-log10|[()]|[)]","",yvar)
        snp_plot <- suppressMessages(
            snp_plot + 
                ggplot2::scale_y_continuous(
                    n.breaks = 3,
                    limits =  c(0,-log10(
                        min(dat[[pval_stripped]], na.rm = TRUE))*1.1)
                )
        )
    }else {
        snp_plot <- suppressMessages(
            snp_plot + ggplot2::scale_y_continuous(breaks = c(0,.5,1),
                                                   limits = c(0,1.15)) +
                ggplot2::labs(y="Fine-mapping PP")
        )
    }
    return(snp_plot)
}
