#' Multi-fine-map summary dot plot
#'
#' @family plot
#' @examples
#' data("BST1")
#' gp <- dot_summary(dat=BST1)
dot_summary <- function(dat,
                        PP_threshold=.95,
                        show_plot=T){
    # library(data.table)
    snp.labs <- construct_snp_labels(dat = dat,
                                      remove_duplicates = F) %>%
        dplyr::arrange(CHR,POS) %>%
        dplyr::mutate(SNP=factor(SNP, levels = unique(SNP), ordered = T))
    CS_cols <- grep(".CS$",colnames(snp.labs), value = T)
    PP_cols <- grep(".PP$",colnames(snp.labs), value = T)
    methodDict <- setNames(gsub("\\.PP","",PP_cols),1:length(PP_cols))
    
    snp.melt <- suppressWarnings(
        data.table::melt.data.table(data = data.table::as.data.table(snp.labs),
                                    id.vars = c("SNP","CHR","POS","leadSNP","Consensus_SNP","color","size","shape"),
                                    measure.vars =  patterns(CS_group=".CS$", PP=".PP$"),
                                    variable.name = c("method")) %>%
            dplyr::mutate(method=factor(methodDict[method], levels = rev(unname(methodDict)), ordered = T),
                          CS=ifelse(CS_group>0,T,NA),
                          PP=ifelse(PP>PP_threshold,PP,NA))
    )
    gp <- ggplot() +
        # Lead SNP
        geom_point(data = subset(snp.melt, leadSNP), aes(x=SNP, y=method),
                   color="red", shape=18, size=2, stroke=1) +
        # CS
        geom_point(data = subset(snp.melt, !is.na(CS)), aes(x=SNP, y=method),
                   color="green3", shape=1, size=3, stroke=1) +
        # Consensus
        geom_point(data = subset(snp.melt, Consensus_SNP), aes(x=SNP, y=method),
                   color="darkgoldenrod1", shape=2, size=5, stroke=1) +
        scale_y_discrete(position = "right") +
        theme_bw()
    if(show_plot)print(gp)
    return(gp)
}
