#' Add vertical lines
#'
#' Adds vertical lines indicates key SNPs
#' @family plot
#' @keywords internal
add_multitrack_lines <- function(TRKS,
                                 dat,
                                 snp_groups=c("Lead","UCS","Consensus"),
                                 line_alpha=1,
                                 line_size=.3,
                                 remove_duplicated_UCS_Consensus=TRUE,
                                 verbose=FALSE){
    messager("+ Adding vertical lines to highlight SNP groups...",v=verbose)
    snp_labels <- construct_snp_labels(dat = dat,
                                        labels_subset = snp_groups,
                                        remove_duplicates = F,
                                        grouping_vars = "SNP")
    ## Remove any lines that are both UCS and Consensus.
    ### Otherwise, when line_alpha<1, they'll make a muddled green/gold color.
    if(remove_duplicated_UCS_Consensus){
        UCS_Cons <- subset(snp_labels, type%in%c("UCS","Consensus"))
        UCS_Cons <- UCS_Cons[base::duplicated(UCS_Cons$SNP),]
        snp_vlines <- rbind(UCS_Cons, subset(snp_labels, type=="Lead"))
    } else {
        snp_vlines <- snp_labels
    }
    
    TRKS_lines <- list()
    # iterate over each ggplot object in a named list
    for(x in names(TRKS)){
        # messager(x, v=verbose)
        trk <- TRKS[[x]]
        genomic_units <- guess_genomic_units(gg = trk)
        if("Consensus" %in% snp_groups){
            consensus.pos <- subset(snp_vlines, type=="Consensus")[[genomic_units]]
            trk <- trk +  # Consensus
                geom_vline(xintercept = consensus.pos, color="goldenrod1",
                           alpha=line_alpha, size=line_size, linetype='solid')
        }
        if("UCS" %in% snp_groups){
            UCS.pos <- subset(snp_vlines, type=="UCS")[[genomic_units]]
            trk <- trk +  # Consensus
                geom_vline(xintercept = UCS.pos, color="green3",
                           alpha=line_alpha, size=line_size, linetype='dashed')
        }
        if("Lead" %in% snp_groups){
            lead.pos <- subset(snp_vlines, type=="Lead")[[genomic_units]]
            trk <- trk +  # Consensus
                geom_vline(xintercept = lead.pos, color="red",
                           alpha=line_alpha, size=line_size, linetype='dashed')
        }
        TRKS_lines[[x]] <- trk
    }
    return(TRKS_lines)
}
