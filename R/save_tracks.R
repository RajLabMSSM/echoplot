save_tracks <- function(locus_dir,
                        TRKS_zoom,
                        window_suffix,
                        LD_reference,
                        split_tracks=FALSE,
                        verbose=TRUE){
    tracks_folder <- file.path(locus_dir,"plot_tracks")
    dir.create(tracks_folder, showWarnings = FALSE, recursive = TRUE)
    locus <- basename(locus_dir)
    if(split_tracks){
        trk_paths <- lapply(names(TRKS_zoom), function(x){
            rds_path <-  file.path(
                tracks_folder, 
                paste(paste(gsub("[:]|[\n]|[/]|[ ]","-",x), "trk",sep="_"),
                      locus,LD_reference,window_suffix, "RDS",sep="."))
            messager("+ echoplot:: Saving ggplots track:",x,"==>",rds_path,
                    v=verbose)
            saveRDS(TRKS_zoom[[x]], rds_path)
            return(x)
        }) |> unlist()
    }else {
        ## NOTE: Saving as one list object somehow takes up about
        ## the same storage space as each track saved individually.
        trk_paths <- rds_path <- file.path(
            tracks_folder, paste("multiview",locus,LD_reference,window_suffix,
                                 "RDS",sep="."))
        messager("+ echoplot:: Saving ggplots tracks list ==>",rds_path, v=verbose)
        saveRDS(TRKS_zoom, rds_path)
    }
    return(trk_paths)
}
