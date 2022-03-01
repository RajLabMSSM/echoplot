#' Generate a locus plot
#'
#' Generate a locus-specific plot with multiple selectable tracks.
#' Users can also generate multiple zoomed in views of the plot at
#'  multiple resolutions.
#' @export 
#' @importFrom dplyr %>%
#' @importFrom echoLD get_lead_r2
#' @importFrom echodata find_consensus_snps fillNA_CS_PP
#' @importFrom echoannot XGR_plot ROADMAP_plot 
#' @importFrom echoannot NOTT2019_plac_seq_plot NOTT2019_epigenomic_histograms
#' @importFrom patchwork wrap_plots plot_annotation
#' @examples
#' dat<- echodata::BST1
#' LD_matrix <- echodata::BST1_LD_matrix
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#'
#' plot_list <- echoplot::plot_locus(dat = dat,
#'                                   locus_dir=locus_dir,
#'                                   LD_matrix=LD_matrix)
plot_locus <- function(dat,
                       locus_dir,
                       LD_matrix=NULL,
                       LD_reference=NULL,
                       dataset_type="GWAS",
                       color_r2=TRUE,
                       method_list=c("ABF","FINEMAP","SUSIE","POLYFUN_SUSIE"),
                       track_order= NULL,
                       track_heights=NULL,
                       plot_full_window=TRUE,
                       dot_summary=FALSE,
                       QTL_prefixes=NULL,
                       mean.PP=TRUE,
                       PP_threshold=.95,
                       consensus_threshold=2,
                       sig_cutoff=5e-8,
                       gene_track=TRUE,
                       point_size=1,
                       point_alpha=.6,
                       snp_group_lines=c("Lead","UCS","Consensus"),
                       xtext=FALSE,
                       show.legend_genes=TRUE,
                       
                       XGR_libnames=NULL,
                       #c("ENCODE_TFBS_ClusteredV3_CellTypes",
                       #   "ENCODE_DNaseI_ClusteredV3_CellTypes",
                       # "Broad_Histone"),
                       n_top_xgr=5,
                       
                       Roadmap=FALSE,
                       Roadmap_query=NULL,
                       n_top_roadmap=7,
                       annot_overlap_threshold=5,
                       zoom_exceptions_str="*full window$|zoom_polygon",
                       
                       Nott_epigenome=FALSE,
                       Nott_regulatory_rects=TRUE,
                       Nott_show_placseq=TRUE,
                       Nott_binwidth=200,
                       Nott_bigwig_dir=NULL,
                       
                       save_plot=TRUE,
                       show_plot=TRUE,
                       genomic_units="Mb",
                       strip.text.y.angle=0,
                       max_transcripts=1,
                       plot.zoom=c("1x"),
                       dpi=300,
                       height=12,
                       width=10,
                       plot_format="jpg",
                       save_RDS=FALSE,
                       return_list=FALSE,
                       conda_env="echoR",
                       nThread=1,
                       verbose=TRUE){
    # library(dplyr); library(ggplot2); LD_reference="UKB";
    # dat<-echolocatoR::BST1; LD_matrix <- echolocatoR::BST1_LD_matrix; locus="BST1";
    # consensus_threshold=2; XGR_libnames=NULL; n_top_xgr=5; mean.PP=T; Roadmap=F; PP_threshold=.95;  Nott_epigenome=T;  save_plot=T; show_plot=T; method_list=c("ABF","SUSIE","POLYFUN_SUSIE","FINEMAP","mean"); full_data=T;  max_transcripts=3; pz=plot.zoom="1x"; dataset_type="GWAS"; dot_summary=F; snp_group_lines=c("UCS","Consensus","Lead"); nThread=4;
    # Nott_epigenome=T; Nott_regulatory_rects=T; Nott_show_placseq=T; Nott_binwidth=200; max_transcripts=1; dpi=400; height=12; width=10; results_path=NULL;  n_top_roadmap=7; annot_overlap_threshold=5; Nott_bigwig_dir=NULL;
    #  Roadmap_query=NULL; sig_cutoff=5e-8; verbose=T; QTL_prefixes=NULL; gene_track=T; genomic_units="Mb";strip.text.y.angle=0; xtext=F; plot_format="jpg"; return_list=F;
    # track_order= c("Genes","GWAS full window","zoom_polygon","GWAS","Fine-mapping", "Roadmap\nchromatin marks\ncell-types", "Nott (2019)\nread densities", "Nott (2019)\nPLAC-seq"); track_heights=NULL; plot_full_window=T;
    
    requireNamespace("ggplot2")
    requireNamespace("patchwork")
    POS <- P <- leadSNP <- NULL;
    
    locus <- basename(locus_dir)
    messager("+-------- Locus Plot: ",locus,"--------+",v=verbose)
    dir.create(locus_dir, showWarnings = FALSE, recursive = TRUE)
    #### Set up data ####
    dat <- echodata::find_consensus_snps(
        dat = dat,
        credset_thresh = PP_threshold,
        consensus_thresh = consensus_threshold,
        verbose = FALSE)
    dat <- echodata::fillNA_CS_PP(dat = dat)
    dat$Mb <- dat$POS/1000000
    
    available_methods <- gsub("\\.PP$","",grep("*\\.PP$",colnames(dat),
                                               value = TRUE)) %>% unique()
    method_list <- unique(method_list[method_list %in% available_methods])
    if(mean.PP){method_list <- unique(c(method_list, "mean"))}
    # Add LD into the dat
    dat <- echoLD::get_lead_r2(
        dat = dat,
        LD_matrix = LD_matrix,
        LD_format = "guess")
    # Begin constructing tracks
    TRKS <- NULL;
    # Track: Summary
    if(dot_summary){
        messager("++ PLOT:: Creating dot plot summary of fine-mapping results.")
        TRKS[["Summary"]] <- dot_summary_plot(dat = dat,
                                              PP_threshold = PP_threshold,
                                              show_plot = FALSE)
    }
    ####  Track: Main (GWAS) frozen ####
    full_window_name <- paste(dataset_type,"full window")
    if(plot_full_window){
        messager("++ PLOT::",dataset_type,"full window track", v=verbose)
        TRKS[[full_window_name]] <- snp_track_merged(
            dat = dat,
              yvar = "-log10(P)",
              sig_cutoff = sig_cutoff,
              labels_subset = NULL,
              xtext = TRUE,
              show.legend = FALSE,
              dataset_type = gsub(" ","\n",full_window_name),
              strip.text.y.angle = strip.text.y.angle,
              verbose = verbose) +
            ggplot2::geom_point(data = subset(dat, leadSNP),
                                ggplot2::aes(x=POS, y=-log10(P)),
                       color="red",pch=9, size=3, 
                       show.legend = FALSE, alpha=1) +
            ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    
    ####  Track: Main (GWAS) ####
    messager("++ PLOT::",dataset_type,"track", v=verbose)
    TRKS[[dataset_type]] <- snp_track_merged(
        dat = dat,
          yvar = "-log10(P)",
          sig_cutoff = sig_cutoff,
          labels_subset = NULL,
          xtext = xtext,
          dataset_type = "GWAS",
          strip.text.y.angle = strip.text.y.angle,
          verbose = verbose) +
        ggplot2::geom_point(data = subset(dat, leadSNP),
                            ggplot2::aes(x=POS, y=-log10(P)),
                   color="red",pch=9, size=3, 
                   show.legend = FALSE, alpha=1)
    
    #### Track: QTL ####
    for (qtl in QTL_prefixes){
        messager("++ PLOT::",qtl,"track", v=verbose)
        pval_col <- guess_pvalue_col(dat, QTL_prefix = qtl)
        TRKS[[qtl]]  <- snp_track_merged(
            dat = dat,
              yvar = paste0("-log10(",pval_col,")"),
              sig_cutoff = sig_cutoff,
              labels_subset = NULL,
              xtext = xtext,
              dataset_type = qtl,
              strip.text.y.angle = strip.text.y.angle,
              verbose = verbose)
    }
    #### Track: Fine-mapping ####
    messager("++ PLOT:: Merged fine-mapping track", v=verbose)
    TRKS[["Fine-mapping"]] <- snp_track_merged(
        dat = dat,
        yvar = "PP",
        sig_cutoff = sig_cutoff,
        absolute_labels = FALSE,
        label_type = "rsid_only",
        labels_subset = c("Lead","CS"),
        show.legend = FALSE,
        xtext = xtext,
        strip.text.y.angle = strip.text.y.angle,
        verbose = verbose)
    ##### Track: Gene Models ####
    # DB tutorial: https://rdrr.io/bioc/ensembldb/f/vignettes/ensembldb.Rmd
    if(gene_track){
        messager("++ PLOT:: Adding Gene model track.",v=verbose)
        try({
            TRKS[["Genes"]] <- transcript_model_track(
                dat = dat,
                   show.legend = show.legend_genes,
                   xtext = xtext,
                   max_transcripts = max_transcripts,
                   expand_x_mult=NULL,
                   verbose=TRUE)
        })
    } 
    #### Track: XGR #### 
    palettes <- c("Spectral","BrBG","PiYG", "PuOr")
    for(i in seq_len(length(XGR_libnames))){
        lib_name <- XGR_libnames[i]
        xgr_out <- echoannot::XGR_plot(dat = dat, 
                                       lib_name = lib_name, 
                                       palette = palettes[i]) 
        TRKS[[lib_name]] <- xgr_out$plot
    } 
    #### Track: Roadmap #### 
    if(Roadmap){
        roadmap_out <- echoannot::ROADMAP_plot(dat = dat, 
                                               roadmap_query = Roadmap_query, 
                                               locus_dir = locus_dir, 
                                               n_top = n_top_roadmap,
                                               conda_env = conda_env, 
                                               nThread = nThread,
                                               verbose = verbose)
        TRKS[["Roadmap\nchromatin marks\ncell-types"]] <- roadmap_out$plot
    }
    #### Track: NOTT2019 ####
    if(Nott_epigenome){
        #### Track: NOTT2019 histogram  ####
        try({ 
            track.Nott_histo <- echoannot::NOTT2019_epigenomic_histograms(
                dat = dat,
                locus_dir = locus_dir,
                geom = "histogram",
                plot_formula = "Cell_type ~.",
                show_plot=FALSE,
                save_plot=FALSE,
                full_data=TRUE,
                return_assay_track=TRUE,
                binwidth=Nott_binwidth,
                bigwig_dir=Nott_bigwig_dir,
                save_annot=TRUE,
                as_ggplot = TRUE,
                strip.text.y.angle = strip.text.y.angle,
                xtext=xtext,
                nThread=nThread,
                verbose=verbose)
            TRKS[["Nott (2019)\nread densities"]] <- track.Nott_histo$plot +
                ggplot2::labs(y="Nott (2019)\nread densities")
        })
        #### Track: NOTT_2019 PLAC-seq  ####
        if(Nott_show_placseq){ 
            try({
                track.Nott_plac <- echoannot::NOTT2019_plac_seq_plot(
                    dat = dat,
                       locus_dir=locus_dir,
                       title=locus,
                       show_regulatory_rects=Nott_regulatory_rects,
                       return_interaction_track=TRUE,
                       show_arches=TRUE,
                       save_annot=TRUE,
                       strip.text.y.angle = strip.text.y.angle,
                       xtext=xtext,
                       nThread=nThread,
                       verbose=verbose)
                TRKS[["Nott (2019)\nPLAC-seq"]] <- track.Nott_plac$plot +
                    ggplot2::labs(y="Nott (2019)\nPLAC-seq")
            })
        }
    } 
    #### Add vertical lines  ####
    if(!is.null(snp_group_lines)){
        TRKS <- add_multitrack_lines(TRKS = TRKS,
                                      dat = dat,
                                      snp_groups = snp_group_lines,
                                      line_alpha = .8,
                                      remove_duplicated_UCS_Consensus = TRUE,
                                      verbose = verbose)
    } 
    ##### Iterate over different window sizes #####
    plot_list <- list()
    for(pz in plot.zoom){
        # try() Allows (X11) errors to occur and still finish the loop
        try({
            messager("+>+>+>+>+ plot.zoom = ",pz," +<+<+<+<+", v=verbose)
            TRKS_zoom <- TRKS
            window_suffix <- get_window_suffix(dat=dat,
                                                    plot.zoom=pz)
            if((plot_full_window) & (!window_suffix %in% c("1x","all"))){
                #### Add zoom polygon ####
                TRKS_zoom[["zoom_polygon"]] <- zoom_polygon(
                    dat = dat,
                     genomic_units = genomic_units,
                     plot.zoom = pz)
                TRKS_zoom[[full_window_name]] <- zoom_highlight(
                    gg = TRKS_zoom[[full_window_name]],
                     dat = dat,
                     plot.zoom = pz)
            }
            #### Remove extra full_window plot #####
            # The steps MUST come before reorder_tracks
            if(window_suffix=="1x"){
                # This track becomes redundant when you don't zoom in at all.
                messager("+ PLOT:: Removing",full_window_name,
                         "track @ zoom=1x",v=verbose)
                TRKS_zoom[[full_window_name]] <- NULL
            }
            
            # WARNING:: The order of these adjustment functions matters!
            ## Some of them reset the parameters of others
            
            #### Remove plots margins to save space ####
            TRKS_zoom <- remove_margins(TRKS = TRKS_zoom,
                                         dat = dat,
                                         verbose = verbose)
            #### Reorder tracks ####
            TRKS_zoom <- reorder_tracks(TRKS = TRKS_zoom,
                                         track_order = track_order,
                                         verbose = verbose)
            #### Make sure last plot has xtext ####
            TRKS_zoom <- add_back_xtext(TRKS = TRKS_zoom,
                                         verbose = verbose)
            #### Define plot.zoom limits ####
            TRKS_zoom <- set_window_limits(TRKS = TRKS_zoom,
                                            dat = dat,
                                            plot.zoom = pz,
                                            exceptions_str=zoom_exceptions_str,
                                            verbose = verbose)
            #### Construct title ####
            n_snps <- if(dataset_type %in% names(TRKS_zoom)){
                paste0("n SNPs: ", 
                       nrow(ggplot2::ggplot_build(
                           TRKS_zoom[[dataset_type]])$data[[2]]),", ")
            } else {NULL}
            title_text <- paste0(basename(locus_dir),"   (",
                                 n_snps,"zoom: ",window_suffix,")")
            
            #### Check track heights ####
            heights <- check_track_heights(TRKS = TRKS_zoom,
                                                track_heights = track_heights,
                                                verbose = verbose)
            #### Fuse all tracks ####
            TRKS_FINAL <- patchwork::wrap_plots(TRKS_zoom,
                                                heights = heights,
                                                ncol = 1) +
                patchwork::plot_annotation(title = title_text)
            
            #### Add plot to list of zoomed plots ####
            plot_list[[pz]] <- if(return_list) TRKS_zoom else TRKS_FINAL
            
            #### Save plot ####
            plot_path <- file.path(
                locus_dir,
                paste("multiview",locus,LD_reference,
                      window_suffix,plot_format,sep="."))
            if(save_plot){
                messager("+ PLOT:: Saving plot ==>",plot_path)
                ggplot2::ggsave(filename = plot_path,
                                plot = TRKS_FINAL,
                                height = height,
                                width = width,
                                dpi = dpi,
                                bg = "transparent")
            }
            
            #### Save ggplot ####
            # Only save one zoom of these since these files are very large
            if(save_RDS & (pz==plot.zoom[1])){
                trk_paths <- save_tracks(locus_dir=locus_dir,
                                         TRKS_zoom=TRKS_zoom,
                                         LD_reference = LD_reference,
                                         window_suffix=window_suffix,
                                         verbose=verbose)
            }
            #### Show the plot ####
            if(show_plot){print(TRKS_FINAL)}
        })
    } # End plot.zoom loop
    return(plot_list)
}
