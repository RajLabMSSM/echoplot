#' Dot summary plot
#' 
#' Multi-fine-map summary dot plot of all lead, UCS and/or Consensus SNPs. 
#'
#' @inheritParams plot_locus
#' @family plot
#' @export
#' @importFrom dplyr arrange mutate
#' @importFrom methods show
#' @importFrom echodata melt_finemapping_results
#' @examples
#' dat <- echodata::BST1
#' gp <- echoplot::dot_summary_plot(dat = dat)
dot_summary_plot <- function(dat,
                             credset_thresh=.95,
                             show_plot=TRUE,
                             verbose=TRUE){
    
    SNP <- CHR <- POS <- Method <- leadSNP <- 
        CS <- Consensus_SNP <- NULL;
    requireNamespace("ggplot2")
    
    messager(
        "++ echoplot:: Creating dot summary plot of fine-mapping results.",
        v=verbose)
    #### Prepare data ####
    finemap_melt <- echodata::melt_finemapping_results(dat = dat, 
                                                       verbose = verbose)
    snp.labs <- construct_snp_labels(dat = finemap_melt,
                                     grouping_vars = c("SNP"),
                                     remove_duplicates = FALSE) |>
        dplyr::arrange(CHR,POS) |>
        dplyr::mutate(SNP=factor(SNP, levels = unique(SNP), ordered = TRUE)) 
    #### Plot ####
    gp <- ggplot2::ggplot() +
        # Lead SNP
        ggplot2::geom_point(data = subset(snp.labs, leadSNP), 
                            ggplot2::aes(x=SNP, y=Method),
                   color="red", shape=18, size=2, stroke=1) +
        # CS
        ggplot2::geom_point(data = subset(snp.labs, !is.na(CS)), 
                            ggplot2::aes(x=SNP, y=Method),
                   color="green3", shape=1, size=3, stroke=1) +
        # Consensus
        ggplot2::geom_point(data = subset(snp.labs, Consensus_SNP), 
                            ggplot2::aes(x=SNP, y=Method),
                   color="darkgoldenrod1", shape=2, size=5, stroke=1) +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::theme_bw()
    if(isTRUE(show_plot)) methods::show(gp)
    return(gp)
}
