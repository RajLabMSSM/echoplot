

check_track_heights <- function(TRKS,
                                track_heights=NULL,
                                default_height=1,
                                verbose=T){
    messager("+ Checking track heights...",v=verbose)
    if(is.null(track_heights)){
        track_heights <- heights_dict(keys=names(TRKS))
    } else {
        # Ensures n TRKS == n track_heights,
        ## even if user-supplied track_heights is > or < than n TRKS.
        track_heights <- track_heights[1:length(TRKS)]
    }
    track_heights <- track_heights[!is.na(track_heights)]
    
    if(length(track_heights)<length(TRKS)){
        messager("Filling in missing track lengths with",default_height,v=verbose)
        n_missing <- length(TRKS)-length(track_heights)
        track_heights <- c(track_heights, rep(default_height,n_missing))
    }
    return(track_heights)
}

