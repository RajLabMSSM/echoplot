#' Plot transcripts
#' 
#' Plot a max number of transcripts per genes.
#' @param dat Data to query transcripts with.
#' @param fill A variable name string, or a color to fill the plot data with.
#' @param shape Shape type to plot genes as. 
#' Passed to \link[Gviz]{plotTracks}.
#' @param transcriptAnnotation Whether to show the gene symbol
#'  or the full transcript ID.
#' Passed to \link[Gviz]{plotTracks}.
#' @param collapseTranscripts Logical or character scalar. 
#' Can be one in gene, longest, shortest or meta. 
#' Merge all transcripts of the same gene into one single gene model. 
#' In the case of gene (or TRUE), this will only keep the start location 
#' of the first exon and the end location of the last exon from all 
#' transcripts of the gene. For shortest and longest, only the longest 
#' or shortest transcript model is retained. For meta, a meta-transcript
#'  containing the union of all exons is formed 
#'  (essentially identical to the operation \code{reduce(geneModel)}).
#' Passed to \link[Gviz]{plotTracks}.
#' @param remove_pseudogenes Whether to remove known pseudogenes.
#' @param method Method to use: "gviz" or "ggplot".
#' @param xtext Include x-axis title and text.
#' @param expand_x_mult Expand the x-axis limits to include partially 
#' overlapping transcripts.
#' @param show.legend Show gene plot legend.
#' @inheritParams plot_locus
#' @inheritParams Gviz::plotTracks
#' @inheritParams Gviz::GeneRegionTrack
#' 
#' @source
#' \href{https://bioconductor.org/packages/release/bioc/vignettes/ensembldb/inst/doc/ensembldb.html}{ensembld tutorial}
#' \href{https://bioconductor.org/packages/devel/bioc/vignettes/Gviz/inst/doc/Gviz.html#45_GeneRegionTrack}{Gvix tutorial}
#' \href{http://bioconductor.org/packages/devel/bioc/vignettes/ggbio/inst/doc/ggbio.pdf}{ggbio tutorial}
#'
#' @export
#' @importFrom echodata dt_to_granges
#' @importFrom stats setNames
#' @importFrom methods show
#' @examples
#' dat <- echodata::BST1
#' gene_track <- echoplot::transcript_model_track(dat=dat)
transcript_model_track <- function(dat,
                                   max_transcripts=1,
                                   tx_biotypes=NULL,
                                   remove_pseudogenes=TRUE,
                                   show_plot=FALSE,
                                   show.legend=FALSE,
                                   fill="skyblue",
                                   shape=c( "arrow", "box", 
                                            "ellipse","smallArrow"),
                                   transcriptAnnotation = c("symbol",
                                                            "transcript"),
                                   collapseTranscripts=c(FALSE,TRUE,"longest"),
                                   stacking=c("squish","hide", "dense",
                                              "pack","full"),
                                   method="ggplot",
                                   xtext=TRUE,
                                   expand_x_mult=c(0.05, .1),
                                   verbose=TRUE){
    # dat <-echolocatoR::LRRK2; max_transcripts=3;
    # method="ggbio"; verbose=T; stacking="squish";  fill="blue"; transcriptAnnotation = c("symbol","transcript");  collapseTranscripts=c(F,T,"longest"); shape="arrow";
    requireNamespace("ggplot2")
    requireNamespace("ggbio") 
    
    gr.snp <- echodata::dt_to_granges(dat = dat,
                                      chrom_col = "CHR", 
                                      start_col = "POS", 
                                      style = "NCBI", 
                                      verbose = verbose)
    #### Gviz ####
    if(method=="gviz"){ 
        requireNamespace("Gviz")
        requireNamespace("EnsDb.Hsapiens.v75")
        requireNamespace("ensembldb")
        requireNamespace("AnnotationFilter")
        requireNamespace("GenomicFeatures")
        
        messager("+ echoplot:: Gene Model Track",v=verbose)
        txdb <-  EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
        tx <- ensembldb::getGeneRegionTrackForGviz(
            txdb,
            filter = AnnotationFilter::GRangesFilter(value = gr.snp,
                                                     feature = "tx"))
        tx_lengths <- GenomicFeatures::transcriptLengths(
            txdb,
            filter = AnnotationFilter::GRangesFilter(value = gr.snp,
                                                     feature = "tx"))
        tx$tx_len <- stats::setNames(tx_lengths$tx_len,
                                     tx_lengths$tx_id)[tx$transcript]
        tx <- subset(tx, !startsWith(as.character(c),"RP11"))
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
                              tx_biotypes = tx_biotypes,
                              remove_pseudogenes = remove_pseudogenes,
                              verbose = verbose)
        #### Handle sitatuions where 0 genes returned ####
        if(length(tx$tx.gr)==0){
            messager("Returning NULL for gene track.",v=verbose)
            return(NULL)
        }
        if(requireNamespace("pals")){
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
                                     ggplot2::aes_string(fill=fill_var,
                                                         color=fill_var),
                                     columns =  c("symbol","tx_id"),
                                     names.expr = names.expr,
                                     color = color,
                                     stat = "identity",
                                     show.legend = show.legend) +
            ggplot2::theme_classic() +
            ggplot2::labs(y="Transcript")
        
        if(!is.null(expand_x_mult)){
            tx.models <- suppressMessages(
                tx.models +
                    # Add some padding at top and bottom
                    ggplot2::scale_y_discrete(
                        expand = ggplot2::expansion(mult = expand_x_mult))
            )
        }
        
        if(xtext==FALSE){
            tx.models <- tx.models +
                ggplot2::theme(axis.text.x =ggplot2:: element_blank(),
                               axis.title.x = ggplot2::element_blank())
        }
        # if(genomic_units=="Mb"){
        #   tx.models <- tx.models +
        #     scale_x_continuous( labels=function(x)x/1000000)
        # }
        if(requireNamespace("pals")){
            # Give it some nice fancy colors
            palette <- unname(pals::alphabet())
            tx.models <- tx.models +
                ggplot2::scale_fill_manual(values = palette) +
                ggplot2::scale_color_manual(values = palette)
        }
        if(method=="ggplot"){
            tx.models <- tx.models@ggplot
        }
    }
    if(show_plot) methods::show(tx.models)
    return(tx.models)
}

