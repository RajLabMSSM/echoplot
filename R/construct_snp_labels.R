#' Construct SNP labels
#'  
#' Construct SNP labels for Manhattan plot.
#' Support function for \link[echoplot]{plot_locus}.
#' @param mean_only_text Only show the text labels with RSIDs in the "mean"
#' Method (not all the other method-specific facets).
#' @param method_specific_consensus Only label Consensus SNPs 
#' that are also Credible Set SNPs for a given method.
#' @inheritParams plot_locus
#' @importFrom dplyr group_by_at arrange slice_max
#' @importFrom data.table data.table as.data.table merge.data.table :=
#' @keywords internal
construct_snp_labels <- function(dat,
                                 labels_subset=c("Lead","CS","Consensus"), 
                                 mean_only_text = c("Consensus","UCS"),
                                 method_specific_consensus=TRUE,
                                 remove_duplicates=TRUE,
                                 grouping_vars=c("SNP"),
                                 merge_with_input=FALSE,
                                 base_size=5,
                                 verbose=FALSE){
    
    rowID <- type <- P <- CS <- leadSNP <- Support <- Consensus_SNP <-
        Method <- NULL;
    
    messager("+ echoplot:: Constructing SNP labels.", v=verbose)
    #### Ensure data.table format ####
    dat <- data.table::as.data.table(dat)
    dat <- echoannot::add_mb(dat = dat)
    #### Get fine-mapping methods ####
    finemap_methods <- if("Method" %in% names(dat)){
        levels(dat$Method)
    } else {
        "all"
    }
    #### Lead SNPs ####
    if("lead" %in% tolower(labels_subset)){
        lead_snps <- subset(dat |> dplyr::arrange(P), leadSNP == TRUE)
        lead_snps$type <- "Lead"
        lead_snps$color <- "red"
        lead_snps$shape <- 9# 18
        lead_snps$size <- base_size
        if("lead" %in% tolower(mean_only_text)){
            lead_snps[,text_label:=(Method=="mean")] 
        } else {
            lead_snps[,text_label:=TRUE] 
        }
    } else {
        lead_snps <- NULL
    }
    #### UCS SNPs (all methods) ####
    if("ucs" %in% tolower(labels_subset)){
        # AFTER fine-mapping
        UCS_snps = subset(dat, Support>0)
        if(dim(UCS_snps)[1]>0){
            UCS_snps$type <- "UCS"
            UCS_snps$color<- "green4"
            UCS_snps$shape <- 16
            UCS_snps$size <- base_size
            UCS_snps$Method <- "mean"
            UCS_snps <- unique(UCS_snps)
        }
        if("ucs" %in% tolower(mean_only_text)){
            UCS_snps[,text_label:=(Method=="mean")] 
        } else {
            UCS_snps[,text_label:=TRUE] 
        }
    } else {
        UCS_snps <- NULL
    }
    #### Credible Set SNPs (method-specific) ####
    CS_snps <- lapply(finemap_methods, 
                      function(m){
        #### Credible Set SNPs ####
        if(("cs" %in% tolower(labels_subset)) &
           ("CS" %in% colnames(dat)) ){
            # AFTER fine-mapping
            if(m=="all"){
                d <- subset(dat, CS>0)
            } else {
                d <- subset(dat, CS>0 & Method==m)
            } 
            if(dim(d)[1]>0){
                d$type <- "CS"
                d$color<- "green3"
                d$shape <- 16
                d$size <- base_size
            }
            if("cs" %in% tolower(mean_only_text)){
                d[,text_label:=(Method=="mean")] 
            }else {
                d[,text_label:=TRUE] 
            }
        } else{
            d <- data.table::data.table()
        } 
        return(d)
    }) |> data.table::rbindlist(fill = TRUE) 
    
    #### Consensus SNPs (all methods) ####
    if(("consensus" %in% tolower(labels_subset)) & 
       ("Consensus_SNP" %in% colnames(dat) ) ){ 
        consensus_SNPs <- subset(dat, Consensus_SNP==TRUE)
        if(dim(consensus_SNPs)[1]>0){
            consensus_SNPs$type <- "Consensus"
            consensus_SNPs$color <- "darkgoldenrod1"
            consensus_SNPs$shape <- 16
            consensus_SNPs$size <- base_size-1
            if("consensus" %in% tolower(mean_only_text)){
                consensus_SNPs[,text_label:=(Method=="mean")] 
            }else {
                consensus_SNPs[,text_label:=TRUE] 
            }
            ## Only include consensus SNPs that are also Credible Set SNPs
            ## for a given method.
            consensus_SNPs <- consensus_SNPs[
                CS>0 | Method %in% c("mean","all")]
        }
    } else {
        consensus_SNPs <- NULL
    }
    
    #### bind all data together ####
    labelSNPs <- data.table::rbindlist(
        list(lead_snps, UCS_snps, CS_snps, consensus_SNPs),
        fill = TRUE) 
    labelSNPs <- labelSNPs[Method=="mean" | (!type %in% mean_only_text),]
    #### If there's duplicates only show the last one ####
    ## Because we've added each label in order of preference 
    labelSNPs$rowID <- seq_len(nrow(labelSNPs)) 
    labelSNPs2 <- (
        labelSNPs |>
            dplyr::group_by_at(.vars = grouping_vars) |> 
            dplyr::arrange(rowID) |>
            dplyr::slice_tail(n = 1) |>
            data.table::data.table()
    )
    #### Choose 1 group per SNP method ####
    if(isTRUE(remove_duplicates)){
        labelSNPs <- labelSNPs2
    #### Choose multiple groups per SNP per method, but only one text label ####
    } else {
        labelSNPs[,text_label:=(rowID %in% labelSNPs2$rowID),]
    }
    labelSNPs$type <- factor(labelSNPs$type,
                             levels = c("UCS","CS","Consensus","Lead"),
                             ordered = TRUE)
    labelSNPs <- dplyr::arrange(labelSNPs, type) 
    #### Merge with input df ####
    if(isTRUE(merge_with_input)){
        plot_dat <- data.table::merge.data.table(
            dat,
            subset(labelSNPs, select=c(grouping_vars,
                                       "type","color","shape","size",
                                       "text_label")),
            by = grouping_vars[grouping_vars %in% colnames(dat)],
            # allow.cartesian = TRUE,
            all.x = TRUE)
        plot_dat[is.na(plot_dat$color),"color"] <- "transparent";
        plot_dat[is.na(plot_dat$shape),"shape"] <- 16;
        plot_dat[is.na(plot_dat$size),"size"] <- 3;
        plot_dat[is.na(plot_dat$text_label),"text_label"] <- FALSE;
        return(plot_dat)
    }else {
        return(labelSNPs)
    }
}
