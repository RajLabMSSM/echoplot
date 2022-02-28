

reorder_tracks <- function(TRKS,
                           track_order=NULL,
                           verbose=T){
    messager("+ Reordering tracks...", v=verbose)
    track_order <- if(is.null(track_order)) names(heights_dict()) else track_order
    
    ## Find user-given tracks that are actually available
    actual_track_order <- track_order[track_order %in% names(TRKS)]
    ## Add back in any tracks user might have missed
    actual_track_order <- unique(c(actual_track_order, names(TRKS)))
    TRKS <- TRKS[actual_track_order]
    return(TRKS)
}
