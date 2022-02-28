#' Plot gene/transcript models
#'
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/ensembldb/inst/doc/ensembldb.html}{ensembld tutorial}
#' \href{https://bioconductor.org/packages/devel/bioc/vignettes/Gviz/inst/doc/Gviz.html#45_GeneRegionTrack}{Gvix tutorial}
#' \href{http://bioconductor.org/packages/devel/bioc/vignettes/ggbio/inst/doc/ggbio.pdf}{ggbio tutorial}
#'
#' @examples
#' data("LRRK2")
#' gene_track <- transcript_model_track(dat=LRRK2)
transcript_model_track <- function(dat,
                                   max_transcripts=1,
                                   remove_pseudogenes=TRUE,
                                   show.legend=TRUE,
                                   show_plot=FALSE,
                                   fill="skyblue",
                                   shape=c( "arrow", "box", "ellipse","smallArrow"),
                                   transcriptAnnotation = c("symbol","transcript"),
                                   collapseTranscripts=c(FALSE,TRUE,"longest"),
                                   stacking=c("squish","hide", "dense", "pack","full"),
                                   method="ggplot",
                                   xtext=TRUE,
                                   expand_x_mult=c(0.05, .1),
                                   verbose=TRUE){
    # dat <-echolocatoR::LRRK2; max_transcripts=3;
    # method="ggbio"; verbose=T; stacking="squish";  fill="blue"; transcriptAnnotation = c("symbol","transcript");  collapseTranscripts=c(F,T,"longest"); shape="arrow";

    gr.snp <- biovizBase::transformDfToGr(data = dat,
                                          seqnames = "CHR",
                                          start = "POS", end = "POS")
    suppressWarnings(GenomeInfoDb::seqlevelsStyle(gr.snp) <- "NCBI")
    #### Gviz ####
    if(method=="gviz"){ 
        requireNamespace("Gviz")
        requireNamespace("EnsDb.Hsapiens.v75")
        messager("+ PLOT:: Gene Model Track",v=verbose)
        txdb <-  EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
        tx <- ensembldb::getGeneRegionTrackForGviz(txdb,
                                                   filter = GRangesFilter(value = gr.snp, feature = "tx"))
        tx_lengths <- transcriptLengths(txdb,
                                        filter = GRangesFilter(value = gr.snp, feature = "tx"))
        tx$tx_len <- setNames(tx_lengths$tx_len,tx_lengths$tx_id)[tx$transcript]
        tx <- subset(tx, !startsWith(as.character(symbol),"RP11"))
        # gtrack <- GenomeAxisTrack()
        tx.models <- Gviz::GeneRegionTrack(tx,
                                           name = "Gene Model",
                                           fill=fill,
                                           stacking = stacking[1])
        # ggplotify::as.ggplot(tx.models)
        ## Problem!: Gviz::plotTracks does not return an actual plot object,
        # no way to convrt this to ggplot
        out <- Gviz::plotTracks(list(tx.models),
                                transcriptAnnotation = transcriptAnnotation[1],
                                background.title = "grey20",
                                collapseTranscripts=collapseTranscripts[1],
                                shape = shape[1])
        
    } else {
        #### ggbio ####
        tx <- get_transcripts(gr.snp = gr.snp,
                              max_transcripts = max_transcripts,
                              remove_pseudogenes = remove_pseudogenes)
        if("pals" %in% row.names(installed.packages())){
            # Ensure teh palette size is always big enough for the
            ## number of unique genes.
            n_genes <- dplyr::n_distinct(tx$gr.snp$symbol)
            palette <- rep(unname(pals::alphabet()),
                           n_genes%/%length(pals::alphabet())+1 )
        }
        if(max_transcripts==1) {
            names.expr <- "symbol"
            fill_var <- NULL
            color <- "darkslateblue"
        }else {
            names.expr <- "symbol" #"symbol   (tx_id)"
            fill_var <- NULL
            color <- "darkslateblue"
        }
        tx.models <- ggbio::autoplot(tx$txdb_filt,
                                     which=tx$tx.gr,
                                     aes_string(fill=fill_var, color=fill_var),
                                     columns =  c("symbol","tx_id"),
                                     names.expr = names.expr,
                                     color = color,
                                     stat = "identity",
                                     show.legend = FALSE) +
            theme_classic() +
            labs(y="Transcript")
        
        if(!is.null(expand_x_mult)){
            tx.models <- suppressMessages(
                tx.models +
                    # Add some padding at top and bottom
                    scale_y_discrete(expand = expansion(mult = expand_x_mult))
            )
        }
        
        
        
        if(xtext==FALSE){
            tx.models <- tx.models +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank())
        }
        # if(genomic_units=="Mb"){
        #   tx.models <- tx.models +
        #     scale_x_continuous( labels=function(x)x/1000000)
        # }
        if(requireNamespace("pals")){
            # Give it some nice fancy colors
            palette <- unname(pals::alphabet())
            tx.models <- tx.models +
                scale_fill_manual(values = palette) +
                scale_color_manual(values = palette)
        }
        if(method=="ggplot"){
            tx.models <- tx.models@ggplot
        }
    }
    if(show_plot) print(tx.models)
    return(tx.models)
}
