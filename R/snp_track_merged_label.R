#' Add SNP labels
#' 
#' Add SNP labels for Manhattan plot.
#' Support function for \link[echoplot]{plot_locus}. 
#' 
#' @inheritParams plot_locus
#' @keywords internal 
#' @importFrom ggnewscale new_scale
snp_track_merged_label <- function(snp_plot, 
                                   yvar="PP",
                                   genomic_units="Mb", 
                                   show.legend=TRUE,
                                   max.overlaps=30,
                                   verbose=TRUE){
    
    requireNamespace("ggplot2")
    requireNamespace("ggrepel")  
    messager("Adding SNP group labels to locus plot.",v=verbose)
    #### Check whether all required cols are present ####
    req_cols <- c("shape","size","color")
    if(!all(req_cols %in% names(snp_plot$data))){
        stp <- paste(
            "All columns must be present in plot data:",
            paste("\n -",shQuote(req_cols), collapse = "")
        )
        stop(stp)
    } 
    #### Add layers #### 
    # gghighlight::gghighlight(label_key = SNP, 
    #                          calculate_per_facet=TRUE)    
    snp_plot2 <- snp_plot + 
        ggnewscale::new_scale(new_aes = "color") + 
        ggplot2::geom_point(data = function(x){x[!is.na(x$type),]},
                            ggplot2::aes_string(pch="shape",
                                                size="size",
                                                color="color",
                                                group="Method"), 
                            fill=NA,
                            show.legend = FALSE) +
        ggplot2::scale_shape_identity() +
        ggplot2::scale_size_identity() +
        ggplot2::scale_color_identity() +
        #### Background color label ####
        ggrepel::geom_label_repel(
            data = function(x){x[!is.na(x$type) & x$text_label,]},
            ggplot2::aes_string(label="SNP"),
            color=NA, 
            fill="black",
            box.padding = .5,
            label.padding = .25,
            point.padding = .5,
            min.segment.length = 100, # omit this segment
            max.overlaps = max.overlaps, 
            label.size=NA,
            alpha=.75,
            seed = 1,
            size = 3,
            show.legend = FALSE) +
        #### Foreground color label ####
        ggrepel::geom_label_repel(
            data = function(x){x[!is.na(x$type) & x$text_label,]},
            ggplot2::aes_string(label="SNP",
                                segment.colour="color"), 
            color="white",
            segment.alpha = .5,
            box.padding = .5,
            label.padding = .25,
            point.padding = .5,
            min.segment.length = 0,
            max.overlaps = max.overlaps, 
            segment.size = .75,
            fill = NA,
            alpha = 1,
            seed = 1,
            size = 3,
            show.legend = FALSE)  + 
        ## Enhance the colors of SNPs with labeled background 
        ##(to see them better)
        ggnewscale::new_scale(new_aes = "color") + 
        ggplot2::geom_point( 
            data = function(x){x[!is.na(x$type),]},
            ggplot2::aes_string(
                x=genomic_units, y=yvar,
                pch="shape",
                color=if("r2" %in% names(snp_plot$data)) "r2" else NULL
            ),
            alpha=1,  
            show.legend = show.legend) +
        ggplot2::scale_color_gradient(low="blue",high ="red",
                                      breaks=c(0,.5,1), limits=c(0,1))
    return(snp_plot2) 
}
