#' Get transcripts
#' 
#' Get transcript positions for all genes in a range.
#' @source
#' \code{
#' #### Alternative approaches I've tried ####
#' dat <- echodata::LRRK2
#' gr.snp <- echodata::dt_to_granges(dat = dat)
#' 
#' # Warning:: MUST load the full package bc
#' # it loads other necessary packages into the namespace.
#' 
#' BiocManager::install("Homo.sapiens")
#' library(Homo.sapiens)
#' txdb <- Homo.sapiens::Homo.sapiens
#' GenomicFeatures::genes(txdb)
#' 
#' BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene")
#' library(TxDb.Hsapiens.UCSC.hg19.knownGene)
#' txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
#' GenomicFeatures::genes(txdb)
#' } 
#' @inheritParams plot_locus
#' @inheritParams get_tx_biotypes
#' @keywords internal
#' @importFrom GenomicRanges seqnames findOverlaps makeGRangesFromDataFrame
#' @importFrom dplyr arrange desc filter row_number ungroup n_distinct
#' @importFrom data.table as.data.table
#' @importFrom ensembldb transcripts listColumns
#' @importFrom AnnotationFilter AnnotationFilterList SeqNameFilter TxIdFilter
#' @importFrom S4Vectors subjectHits
get_transcripts <- function(gr.snp,
                            max_transcripts=1,
                            remove_pseudogenes=TRUE,
                            tx_biotypes=NULL,
                            verbose=TRUE){
    symbol <- tx_name <- tx_biotype <- width <- NULL;
    requireNamespace("EnsDb.Hsapiens.v75")  
    
    ##### Check transcript biotypes #####
    if(!is.null(tx_biotypes)){
        tx_filter <- get_tx_biotypes(tx_biotypes=tx_biotypes,
                                     verbose=verbose)
    } else {
        tx_filter <- NULL
    }
    txdb <- EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
    #### Query genome db with finemap_DT coordinates ####
    txdb_transcripts <- ensembldb::transcripts(
        txdb,
      columns = c(ensembldb::listColumns(txdb ,"tx"),"symbol"),
      filter = AnnotationFilter::AnnotationFilterList(
          #### Filter by seqnames ####
          AnnotationFilter::SeqNameFilter(
              value = unique(GenomicRanges::seqnames(gr.snp))
              ),
          #### Filter by tx_biotype ####
          tx_filter
          )
      ) 
    #### Find exact overlap between transcripts and finemap_DT ####
    # PROBLEM: This
    hits <- GenomicRanges::findOverlaps(query = gr.snp,
                                        subject = txdb_transcripts,
                                        type="any",
                                        ignore.strand=TRUE)
    #### limit the number of transcript per gene ####
    tx.filt <- data.frame(txdb_transcripts[S4Vectors::subjectHits(hits),]) |>
        ## !!IMPORTANT!! If unique() isn't applied here,
        ## dplyr will pick duplicates of the same transcript
        unique() |>
        subset((!is.na(symbol)) & (!is.na(tx_name))) |>
        dplyr::group_by(symbol) |>
        # This operation does not involve huge datasets
        # group & arrange & filter should be fine in terms of speed
        dplyr::arrange(dplyr::desc(width)) |> 
        dplyr::filter(dplyr::row_number() %in% seq_len(max_transcripts)) |>
        dplyr::ungroup() |> 
        data.table::as.data.table()
    #### remove_pseudogenes ####
    if(isTRUE(remove_pseudogenes)){
        pseudo <- grep("pseudogene|nonsense_mediated_decay",
                       unique(tx.filt$tx_biotype),value = TRUE)
        tx.filt <- subset(tx.filt,
                          (!tx_biotype %in% pseudo) &
                              (!startsWith(symbol,"RP11")) &
                              (!startsWith(symbol,"RPS"))
        )
    }
    #### Convert back to Granges #### 
    tx.gr <- GenomicRanges::makeGRangesFromDataFrame(
        df = tx.filt, 
        seqnames.field = "seqnames",
        start.field = "start", 
        end.field = "end",
        keep.extra.columns = TRUE)
    #### Filter tx database ####
    # Just return the full txdb (filtered version causes issues w ggbio)
    # IMPORTANT! you must use the filteed db when plotting
    ## bc otherwise ggbio will plot all overlapping transcripts
    # (not just the ones you selected here).
    txdb_filt <- ensembldb::filter(
        txdb,
        filter = AnnotationFilter::AnnotationFilterList(
            AnnotationFilter::TxIdFilter(value=tx.filt$tx_id)
            )
    )
    #### Report ####
    messager("max_transcripts=",max_transcripts,". ", 
             v=verbose)
    messager(dplyr::n_distinct(tx.gr$tx_id)," transcripts from ",
             dplyr::n_distinct(tx.gr$symbol)," genes returned.",
            v=verbose)
    return(list(txdb_filt=txdb_filt,
                tx.gr=tx.gr))
}
