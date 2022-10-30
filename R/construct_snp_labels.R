#' Construct SNP labels
#'  
#' Construct SNP labels for Manhattan plot.
#' Support function for \link[echoplot]{plot_locus}.
#' @inheritParams plot_locus
#' @importFrom dplyr group_by arrange slice vars n
#' @importFrom data.table data.table as.data.table merge.data.table
#' @keywords internal
construct_snp_labels <- function(dat,
                                  labels_subset=c("Lead","CS","UCS",
                                                  "Consensus"),
                                  remove_duplicates=TRUE,
                                  grouping_vars=c("SNP"),
                                  merge_with_input=FALSE,
                                  verbose=FALSE){
    rowID <- type <- P <- leadSNP <- CS <- Support <- Consensus_SNP <- NULL;
    
    messager("+ echoplot:: Constructing SNP labels...", v=verbose)
    labelSNPs <- data.table::data.table()
    dat <- data.table::as.data.table(dat)
    dat$Mb <- dat$POS/1000000
    
    ## BEFORE fine-mapping
    if("lead" %in% tolower(labels_subset)){
        lead_snps <- subset(dat |> dplyr::arrange(P), leadSNP == TRUE)
        lead_snps$type <- "Lead"
        lead_snps$color <- "red"
        lead_snps$shape <- 9# 18
        lead_snps$size <- 3
        labelSNPs <- rbind(labelSNPs, lead_snps, fill=TRUE)
    }
    if(("cs" %in% tolower(labels_subset)) & ("CS" %in% colnames(dat) ) ){
        # AFTER fine-mapping
        CS_snps = subset(dat, CS>0)
        if(dim(CS_snps)[1]>0){
            CS_snps$type <- "CS"
            CS_snps$color<- "green3"
            CS_snps$shape <- 16
            CS_snps$size=5
            labelSNPs <- rbind(labelSNPs, CS_snps, fill=TRUE)
        }
    }
    if("ucs" %in% tolower(labels_subset)){
        # AFTER fine-mapping
        UCS_snps = subset(dat, Support>0)
        if(dim(UCS_snps)[1]>0){
            UCS_snps$type <- "UCS"
            UCS_snps$color<- "green3"
            UCS_snps$shape <- 16
            UCS_snps$size=5
            labelSNPs <- rbind(labelSNPs, UCS_snps, fill=TRUE)
        }
    }
    if(("consensus" %in% tolower(labels_subset)) & 
       ("Consensus_SNP" %in% colnames(dat) ) ){
        # Conensus across all fine-mapping tools
        consensus_SNPs <- subset(dat, Consensus_SNP==TRUE)
        if(dim(consensus_SNPs)[1]>0){
            consensus_SNPs$type <- "Consensus"
            consensus_SNPs$color <- "darkgoldenrod1"
            consensus_SNPs$shape <- 16
            consensus_SNPs$size=6
            labelSNPs <- rbind(labelSNPs, consensus_SNPs, fill=TRUE)
        }
    }
    # If there's duplicates only show the last one
    labelSNPs$rowID <- seq_len(nrow(labelSNPs))
    if(remove_duplicates){
        labelSNPs <- labelSNPs |>
            dplyr::group_by(dplyr::vars(grouping_vars)) |>
            dplyr::arrange(rowID) |>
            dplyr::slice(dplyr::n())
    }
    labelSNPs$type <- factor(labelSNPs$type,
                             levels = c("UCS","CS","Consensus","Lead"),
                             ordered = TRUE)
    labelSNPs <- dplyr::arrange(labelSNPs, type)
    if("Method"%in%colnames(labelSNPs)){
        labelSNPs$Method <- factor(labelSNPs$Method, 
                                   unique(labelSNPs$Method),
                                   ordered = TRUE)
    } 
    #### Merge with input df ####
    if(merge_with_input){
        plot_dat <- data.table::merge.data.table(
            dat,
            subset(labelSNPs, select=c("SNP","Method","type",
                                       "color","shape","size")),
            by = c("SNP","Method")[c("SNP","Method") %in% colnames(dat)],
            all.x = TRUE)
        plot_dat[is.na(plot_dat$color),"color"] <- "transparent";
        plot_dat[is.na(plot_dat$shape),"shape"] <- 16;
        plot_dat[is.na(plot_dat$size),"size"] <- 3;
        return(plot_dat)
    }else {return(as.data.frame(labelSNPs))}
}
