get_transcripts <- function(gr.snp,
                            max_transcripts=1,
                            remove_pseudogenes=TRUE,
                            verbose=TRUE){
    # gr.snp <- GenomicRanges::makeGRangesFromDataFrame(echolocatoR::LRRK2, keep.extra.columns = T, seqnames.field = "CHR", start.field = "POS", end.field = "POS")
    
    # Warning:: MUST load the full package bc
    # it loads other necessary packages into the namespace.
    
    # BiocManager::install("Homo.sapiens")
    # library(Homo.sapiens)
    # txdb <- Homo.sapiens::Homo.sapiens
    # GenomicFeatures::genes(txdb)
    
    # BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene")
    # library(TxDb.Hsapiens.UCSC.hg19.knownGene)
    # txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
    # GenomicFeatures::genes(txdb)
    
    symbol <- tx_name <- tx_biotype <- NULL;
    requireNamespace("EnsDb.Hsapiens.v75") 
    requireNamespace("AnnotationFilter")
    requireNamespace("ensembldb")
    requireNamespace("S4Vectors")
    
    txdb <- EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
    #### Query genome db with finemap_DT coordinates ####
    txdb_transcripts <- ensembldb::transcripts(
        txdb,
      columns = c(ensembldb::listColumns(txdb , "tx"), "symbol"),
      filter = AnnotationFilter::AnnotationFilterList(
          # GRangesFilter(gr.snp)
          AnnotationFilter::SeqNameFilter(
              value = unique(GenomicRanges::seqnames(gr.snp)) )
                                              ))
    #### Find exact overlap between transcripts and finemap_DT ####
    # PROBLEM: This
    hits <- GenomicRanges::findOverlaps(query = gr.snp,
                                        subject = txdb_transcripts,
                                        type="any",
                                        ignore.strand=TRUE)
    
    #### limit the number of transcript per gene ####
    tx.filt <- data.frame(txdb_transcripts[S4Vectors::subjectHits(hits),]) %>%
        ## !!IMPORTANT!! If unique() isn't applied here,
        ## dplyr will pick duplicates of the same transcript
        unique() %>%
        subset((!is.na(symbol)) & (!is.na(tx_name))) %>%
        dplyr::group_by(symbol) %>%
        # slice_max does not behave as expected.
        # If you don't explicitly group by the group var,
        ## you will only return the top n transcripts overall,
        ## regardless of genes (really dumb...)
        dplyr::slice_max(order_by = symbol, #c(symbol,width),
                         n = max_transcripts,
                         with_ties = FALSE) %>%
        data.table::as.data.table()
    
    #### remove_pseudogenes ####
    if(remove_pseudogenes){
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
        start.field = "start", end.field = "end",
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
