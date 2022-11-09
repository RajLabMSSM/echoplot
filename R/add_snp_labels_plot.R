add_snp_labels_plot <- function(snp_plot,
                                snp_labels=NULL,
                                yvar,
                                genomic_units,
                                show.legend=TRUE){
    #### Extract data ####
    # if(is.null(snp_labels)){
    #     snp_labels <- snp_plot$data
    # }
    #### Check whether all required cols are present ####
    req_cols <- c("shape","size","color")
    if(!all(req_cols %in% names(snp_labels))){
        stp <- paste(
            "All columns must be present in plot data:",
            paste("\n -",shQuote(req_cols), collapse = "")
        )
        stop(stp)
    } 
    #### Add layers #### 
    # gghighlight::gghighlight(label_key = SNP, 
    #                          calculate_per_facet=TRUE)  
    snp_plot <- snp_plot +
        ggplot2::geom_point(data = function(x){x[!is.na(x$type),]},
                           ggplot2::aes_string(pch="shape",
                                               size="size",
                                               color="color",
                                               group="Method"), 
                            fill=NA) +
        ggplot2::scale_shape_identity() +
        ggplot2::scale_size_identity() +
        ggplot2::scale_color_identity() +
        ### Background color label
        ggrepel::geom_label_repel(data = function(x){x[!is.na(x$type),]},
                                  ggplot2::aes_string(label="SNP" ),
                                  color=NA,
                                  # nudge_x = .5,
                                  fill="black",
                                  box.padding = .5,
                                  label.padding = .25,
                                  point.padding = .5,
                                  max.overlaps = 30, 
                                  label.size=NA,
                                  alpha=.75,
                                  seed = 1,
                                  size = 3) +
        ### Foreground color label
        ggrepel::geom_label_repel(data = function(x){x[!is.na(x$type),]},
                                  ggplot2::aes_string(label="SNP",
                                                      segment.colour="color"), 
                                  color="white",
                                  segment.alpha = .5,
                                  box.padding = .5,
                                  label.padding = .25,
                                  point.padding = .5,
                                  min.segment.length = 0,
                                  max.overlaps = 30, 
                                  segment.size = .75,
                                  fill = NA,
                                  alpha = 1,
                                  seed = 1,
                                  size = 3)  + 
    ## Enhance the colors of SNPs with labeled background 
       ##(to see them better)
        ggplot2::geom_point( 
            data = function(x){x[!is.na(x$type),]},
            ggplot2::aes_string(
                x=genomic_units, y=yvar,
                pch="shape",
                color=if("r2" %in% names(snp_labels)) "r2" else NULL
            ),
            alpha=1,  
            show.legend = show.legend)
    return(snp_plot)
}
