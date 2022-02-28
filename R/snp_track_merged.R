
snp_track_merged <- function(dat,
                              yvar="-log10(P)",
                              labels_subset = c("Lead","UCS","Consensus"),
                              absolute_labels=FALSE,
                              label_type="rsid_only",
                              sig_cutoff=5e-8,
                              cutoff_lab=paste("P <",sig_cutoff),
                              point_alpha=.5,
                              show.legend=TRUE,
                              xtext=TRUE,
                              facet_formula="Method~.",
                              dataset_type=NULL,
                              genomic_units="POS",
                              strip.text.y.angle=0,
                              show_plot=FALSE,
                              verbose=TRUE){
    # labels_subset = c("Lead","UCS","Consensus"); yvar="-log10(P)"; absolute_labels=F; label_type="rsid_only";  sig_cutoff=5e-8;
    # cutoff_lab=paste("P <",sig_cutoff); point_alpha=.5; show.legend=T; xtext=T;  facet_formula="Method~."; track_heights=NULL;
    if(endsWith(yvar,"PP")) {
        finemap_melt <- echodata::melt_finemapping_results(
            dat = dat,
            verbose = verbose)
        yvar <- "PP"
        sig_cutoff <- .95
        cutoff_lab <- paste("PP ≥",sig_cutoff)
        remove_duplicates <- FALSE
        melt_methods <- TRUE
        grouping_vars <- c("SNP","Method")
    } else {
        finemap_melt <- dat
        finemap_melt$Method <- if(is.null(dataset_type)) yvar else dataset_type
        cutoff_lab <- paste("P <",sig_cutoff)
        sig_cutoff <- -log10(sig_cutoff)
        remove_duplicates <- TRUE
        melt_methods <- FALSE
        grouping_vars <- c("SNP")
    }
    finemap_melt$Mb <- finemap_melt$POS/1000000
    
    # Plot
    snp_plot <- ggplot(data = finemap_melt,
                       aes_string(x=genomic_units, y=yvar, color="r2")) +
        # Bottom plot delineator
        geom_hline(yintercept=0) +
        geom_point(alpha=point_alpha, show.legend = show.legend) +
        scale_color_gradient(low="blue",high ="red",
                             breaks=c(0,.5,1), limits=c(0,1)) +
        ## Sig cutoff line
        geom_hline(yintercept=sig_cutoff, alpha=.5, linetype=2, size=.5, 
                   color="black") +
        geom_text(aes(x=min(Mb), y=sig_cutoff*1.1),
                  label=cutoff_lab,
                  size=3,color="grey", hjust = 0) +
        labs(color=bquote(r^2),
             y=if(startsWith(yvar,"-log10")){
                 bquote("-log"[10]~"(p)")
             } else {yvar} ) +
        theme_classic() +
        facet_grid(facets =if(is.null(facet_formula)) {
            facet_formula
            } else  {as.formula(facet_formula)}) +
        theme(strip.text.y = element_text(angle=strip.text.y.angle)
        ) +
        guides(fill = guide_colourbar(barwidth = 1, barheight = 3))
    
    #  Choose breaks and ylims
    if(startsWith(yvar,"-log")){
        pval_stripped <- gsub("-log|-log10|[()]|[)]","",yvar)
        snp_plot <- suppressMessages(
            snp_plot + 
                scale_y_continuous(
                    n.breaks = 3,
                    limits =  c(0,-log10(
                        min(dat[[pval_stripped]], na.rm = TRUE))*1.1)
                    )
        )
    }else {
        snp_plot <- suppressMessages(
            snp_plot + scale_y_continuous(breaks = c(0,.5,1),
                                          limits = c(0,1.15)) +
                labs(y="Fine-mapping PP")
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
    if(xtext==F){
        snp_plot <- snp_plot +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank())
    }
    if(show_plot) print(snp_plot)
    return(snp_plot)
}

