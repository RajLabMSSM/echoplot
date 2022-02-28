

add_snp_labels <- function(snp_plot,
                           dat,
                           labels_subset=c("CS"),
                           yvar="PP",
                           genomic_units="Mb",
                           grouping_vars=c("SNP","Method"),
                           remove_duplicates=F,
                           melt_methods=T,
                           show.legend=T){
    if(melt_methods){
        finemap_melt <- echodata::melt_finemapping_results(
            dat = dat,
            verbose = FALSE)
    } else {finemap_melt <- dat}
    snp_points <- construct_snp_labels(dat = finemap_melt,
                                        labels_subset = c("Lead","UCS","Consensus"),
                                        remove_duplicates = remove_duplicates,
                                        grouping_vars = grouping_vars)
    if(melt_methods){
        snp_labels <- construct_snp_labels_separate(finemap_melt = finemap_melt,
                                                     labels_subset = labels_subset)
    } else {snp_labels <- snp_points}
    # Add SNP labels to plot
    snp_plot_labeled <- snp_plot +
        geom_point(data=snp_labels,
                   pch=snp_labels$shape,
                   fill=NA,
                   size=snp_labels$size,
                   color=snp_labels$color) +
        ### Background color label
        ggrepel::geom_label_repel(data=snp_labels,
                                  aes(label=SNP),
                                  color=NA,
                                  # nudge_x = .5,
                                  fill="black",
                                  box.padding = .5,
                                  label.padding = .25,
                                  point.padding = .5,
                                  # min.segment.length = 1,
                                  # nudge_x = if(absolute_labels).5 else 0,
                                  # nudge_y= if(absolute_labels).5 else 0,
                                  label.size=NA,
                                  alpha=.75,
                                  seed = 1,
                                  size = 3) +
        ### Foreground color label
        ggrepel::geom_label_repel(data=snp_labels,
                                  aes(label=SNP),
                                  segment.colour = snp_labels$color,
                                  color="white",#labelSNPs_noDupes$color,
                                  segment.alpha = .5,
                                  box.padding = .5,
                                  label.padding = .25,
                                  point.padding = .5,
                                  # min.segment.length = 1,
                                  # nudge_x = if(absolute_labels).5 else NULL,
                                  # nudge_y= if(absolute_labels).5 else NULL,
                                  segment.size = .75,
                                  fill = NA,
                                  alpha=1,
                                  seed = 1,
                                  size = 3) +
        # Enhance the colors of SNPs with labeled background (to see them better)
        geom_point(data =snp_points,
                   aes_string(x=genomic_units, y=yvar,color="r2"),
                   alpha=1, pch=snp_points$shape,
                   show.legend = show.legend)
    return(snp_plot_labeled)
}
