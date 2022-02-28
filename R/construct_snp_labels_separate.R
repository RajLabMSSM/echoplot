


construct_snp_labels_separate <- function(finemap_melt,
                                           labels_subset="CS"){
    # Process means and non-means separately so you can add extra labels to mean
    finemap_labels <- construct_snp_labels(dat = finemap_melt,
                                            labels_subset = labels_subset,
                                            grouping_vars = c("SNP","Method")) %>%
        subset(Method!="mean")
    mean_labels <- construct_snp_labels(dat = finemap_melt,
                                         labels_subset = c("Lead","UCS","Consensus"),
                                         grouping_vars = c("SNP","Method"))  %>%
        subset(Method=="mean")
    mod_labels <- rbind(finemap_labels, mean_labels)
    return(mod_labels)
}
