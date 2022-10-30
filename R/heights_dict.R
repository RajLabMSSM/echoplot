

heights_dict <- function(keys=NULL,
                         default_height=1){
    heights_dict <- c(
        "GWAS full window"=.33,
        "QTL full window"=.33,
        "zoom_polygon"=.15,
        "Genes"=.5,
        "GWAS"=.33,
        "QTL"=.33,
        "Fine-mapping"=1,
        "Roadmap\nchromatin marks\ncell-types"=1,
        "Nott (2019)\nread densities"=.5,
        "Nott (2019)\nPLAC-seq"=.5)
    if(is.null(keys)) return(heights_dict)
    # Query dict
    heights <- lapply(keys, function(k){
        if(k %in% names(heights_dict)){
            return(heights_dict[[k]])
        }else return(default_height)
    }) |> unlist()
    return(heights)
}
