#' SNP track merged
#' 
#' Manhattan plot of GWAS/QTL data with various fine-mapping 
#' related annotations. Support function for \link[echoplot]{plot_locus}. 
#' @keywords internal
#' @inheritParams plot_locus
#' @importFrom echodata melt_finemapping_results
#' @importFrom echoannot add_mb
#' @importFrom stats as.formula
#' @importFrom methods show
#' @export
#' @examples 
#' dat <- echodata::BST1[seq_len(100),]
#' plt <- snp_track_merged(dat = dat)
snp_track_merged <- function(dat,
                             yvar="-log10(P)",
                             labels_subset = c("Lead","UCS","Consensus"),
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
                             strip.text.y.angle=0,
                             show_plot=FALSE,
                             verbose=TRUE){
    # echoverseTemplate:::args2vars(snp_track_merged)
    # echoverseTemplate:::source_all()
    requireNamespace("ggplot2") 
    
    #### Prepare data ####
    if(endsWith(yvar,"PP")) {
        finemap_melt <- echodata::melt_finemapping_results(
            dat = dat,
            verbose = verbose)
        yvar <- "PP"
        sig_cutoff <- .95
        cutoff_lab <- paste("PP >=",sig_cutoff)
        remove_duplicates <- FALSE
        melt_methods <- TRUE
        grouping_vars <- c("SNP","Method")
    } else {
        finemap_melt <- dat
        finemap_melt$Method <- if(is.null(dataset_type)) yvar else dataset_type
        cutoff_lab <- paste0("P<",formatC(sig_cutoff))
        sig_cutoff <- -log10(sig_cutoff)
        remove_duplicates <- TRUE
        melt_methods <- FALSE
        grouping_vars <- c("SNP")
    }
    finemap_melt <- echoannot::add_mb(dat = finemap_melt)
    #### Plot #####
    snp_plot <- ggplot2::ggplot(
        data = finemap_melt,
        ggplot2::aes_string(
            x=genomic_units, y=yvar, 
            color=if("r2" %in% names(finemap_melt)) "r2" else NULL
        )) +
        # Bottom plot delineator
        ggplot2::geom_hline(yintercept=0) +
        ggplot2::geom_point(alpha=point_alpha, show.legend = show.legend) +
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
    
    #  Choose breaks and ylims
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
    if(!is.null(labels_subset)){
        snp_plot <- add_snp_labels(snp_plot = snp_plot,
                                    dat = dat,
                                    yvar = yvar,
                                    genomic_units = genomic_units,
                                    labels_subset = labels_subset,
                                    remove_duplicates = remove_duplicates,
                                    melt_methods = melt_methods,
                                    grouping_vars = grouping_vars,
                                    show.legend = show.legend)
    }
    if(xtext==FALSE){
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


