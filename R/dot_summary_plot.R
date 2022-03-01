#' Dot summary plot
#' 
#' Multi-fine-map summary dot plot of all lead, UCS and/or Consensus SNPs. 
#'
#' @inheritParams plot_locus
#' @family plot
#' @export
#' @importFrom data.table melt.data.table
#' @importFrom dplyr %>% arrange mutate
#' @examples
#' dat <- echodata::BST1
#' gp <- echoplot::dot_summary_plot(dat=dat)
dot_summary_plot <- function(dat,
                             PP_threshold=.95,
                             show_plot=TRUE){
    SNP <- CS_group <- PP <- CHR <- POS <- method <- leadSNP <- 
        CS <- Consensus_SNP <- NULL;
    requireNamespace("ggplot2")
    
    snp.labs <- construct_snp_labels(dat = dat,
                                      remove_duplicates = FALSE) %>%
        dplyr::arrange(CHR,POS) %>%
        dplyr::mutate(SNP=factor(SNP, levels = unique(SNP), ordered = TRUE))
    # CS_cols <- grep(".CS$",colnames(snp.labs), value = TRUE)
    PP_cols <- grep(".PP$",colnames(snp.labs), value = TRUE)
    methodDict <- setNames(gsub("\\.PP","",PP_cols),seq_len(length(PP_cols)) )
    
    snp.melt <- suppressWarnings(
        data.table::melt.data.table(
            data = data.table::as.data.table(snp.labs),
            id.vars = c("SNP","CHR","POS","leadSNP","Consensus_SNP","color","size","shape"),
            measure.vars =  data.table:::patterns(CS_group=".CS$", PP=".PP$"),
            variable.name = c("method")) %>%
            dplyr::mutate(method=factor(methodDict[method],
                                        levels = rev(unname(methodDict)), 
                                        ordered = TRUE),
                          CS=ifelse(CS_group>0,TRUE,NA),
                          PP=ifelse(PP>PP_threshold,PP,NA))
    )
    gp <- ggplot2::ggplot() +
        # Lead SNP
        ggplot2::geom_point(data = subset(snp.melt, leadSNP), 
                            ggplot2::aes(x=SNP, y=method),
                   color="red", shape=18, size=2, stroke=1) +
        # CS
        ggplot2::geom_point(data = subset(snp.melt, !is.na(CS)), 
                            ggplot2::aes(x=SNP, y=method),
                   color="green3", shape=1, size=3, stroke=1) +
        # Consensus
        ggplot2::geom_point(data = subset(snp.melt, Consensus_SNP), 
                            ggplot2::aes(x=SNP, y=method),
                   color="darkgoldenrod1", shape=2, size=5, stroke=1) +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::theme_bw()
    if(show_plot)print(gp)
    return(gp)
}
