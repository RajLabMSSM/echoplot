add_back_xtext <- function(TRKS,
                           verbose=TRUE){
    requireNamespace("ggplot2")
    
    messager("+ Ensuring last track shows genomic units.",v=verbose)
    i <- length(TRKS)
    unit_divisor <- if(guess_genomic_units(gg = TRKS[[i]])=="Mb") 1 else 1000000
    TRKS[[i]] <- suppressMessages(
        TRKS[[i]] + ggplot2::theme(axis.text.x = ggplot2::element_text(),
                          axis.title.x = ggplot2::element_text()) +
            ggplot2::labs(x="Mb") +
            ggplot2::scale_x_continuous(labels = function(x)x/unit_divisor)
    )
    return(TRKS)
}