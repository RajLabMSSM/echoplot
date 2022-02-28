
get_max_histogram_height <- function(gg,
                                     round_to=NULL,
                                     verbose=T){
    if(tolower(class(gg)[1])=="ggbio") gg <- gg@ggplot
    messager("+ PLOT:: Calculating max histogram height",v=verbose)
    dat <- ggplot2::ggplot_build(gg)$data[[1]]
    max_height <- max(dat$ymax)
    if(!is.null(round_to)){
        max_height <- DescTools::RoundTo(max_height, round_to)
    }
    return(max_height)
}
