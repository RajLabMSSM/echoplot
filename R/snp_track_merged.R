#' SNP track merged
#' 
#' Manhattan plot of GWAS/QTL data with various fine-mapping 
#' related annotations. Support function for \link[echoplot]{plot_locus}. 
#' @param remove_duplicates Remove duplicate labels when SNPs are part of >1 
#' group in \code{labels_subset}.
#' @keywords internal
#' @inheritParams plot_locus
#' @importFrom echodata melt_finemapping_results
#' @importFrom echoannot add_mb
#' @importFrom stats as.formula
#' @importFrom methods show
#' @export
#' @examples 
#' dat <- echodata::BST1[seq_len(100),]
#' plt <- snp_track_merged(dat = dat, yvar="PP", show_plot=TRUE)
snp_track_merged <- function(dat,
                             yvar="-log10(P)",
                             labels_subset = c("Lead","CS","UCS","Consensus"),
                             absolute_labels=FALSE,
                             label_type="rsid_only",
                             label_leadsnp=TRUE,
                             sig_cutoff=5e-8, 
                             point_alpha=.5,
                             show.legend=TRUE,
                             xtext=TRUE,
                             facet_formula="Method~.",
                             dataset_type=NULL,
                             genomic_units="POS",
                             remove_duplicates=FALSE,
                             strip.text.y.angle=0,
                             show_plot=FALSE,
                             verbose=TRUE){
    # devoptera::args2vars(snp_track_merged)
    
    requireNamespace("ggplot2") 
    leadSNP <- NULL;
    
    #### Prepare data ####
    if(endsWith(yvar,"PP")) {
        finemap_melt <- echodata::melt_finemapping_results(
            dat = dat,
            verbose = verbose)
        yvar <- "PP"
        sig_cutoff <- .95
        cutoff_lab <- paste("PP >=",sig_cutoff)
        melt_methods <- TRUE
        grouping_vars <- c("SNP","Method")
    } else {
        finemap_melt <- dat
        finemap_melt$Method <- if(is.null(dataset_type)) yvar else dataset_type
        cutoff_lab <- paste0("P<",formatC(sig_cutoff))
        sig_cutoff <- -log10(sig_cutoff) 
        melt_methods <- FALSE
        grouping_vars <- c("SNP")
    } 
    finemap_melt <- echoannot::add_mb(dat = finemap_melt)
    #### Get grouping vars from facet formula ####
    grouping_vars <- c("SNP",
                       formula_terms(formula_str = facet_formula))
    #### add SNP labels ####
    if(!is.null(labels_subset)){
        finemap_melt <- construct_snp_labels(
            dat = finemap_melt,
            labels_subset = labels_subset,
            remove_duplicates = remove_duplicates,
            mean_only_text = c("UCS"),
            grouping_vars = grouping_vars,
            merge_with_input = TRUE,
            verbose = verbose) 
    } 
    #### Get info on SNPs ####
    # subset(finemap_melt, !is.na(type)) |> dplyr::group_by(Method) |> count()
    #### Plot #####
    snp_plot <- snp_track_merged_base(finemap_melt = finemap_melt, 
                                      genomic_units = genomic_units, yvar = yvar, 
                                      point_alpha = point_alpha,
                                      show.legend = show.legend,
                                      sig_cutoff = sig_cutoff, 
                                      cutoff_lab = cutoff_lab, 
                                      facet_formula = facet_formula, 
                                      strip.text.y.angle = strip.text.y.angle)  
    if(!is.null(labels_subset)){
        snp_plot <- snp_track_merged_label(snp_plot = snp_plot, 
                                           yvar = yvar,
                                           genomic_units = genomic_units, 
                                           show.legend = show.legend,
                                           verbose = verbose) 
    }
    if(isFALSE(xtext)){
        snp_plot <- snp_plot +
            ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                  axis.title.x = ggplot2::element_blank())
    }
    if(isTRUE(label_leadsnp)){
        snp_plot <- snp_plot + 
            ggplot2::geom_point(data = subset(finemap_melt, leadSNP),
                                ggplot2::aes_string(x=genomic_units,
                                                    y=yvar),
                                color="red",pch=9, size=3, 
                                show.legend = FALSE, alpha=1) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if(isTRUE(show_plot)) methods::show(snp_plot)
    return(snp_plot)
}


