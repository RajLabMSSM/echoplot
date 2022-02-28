
add_back_xtext <- function(TRKS,
                           verbose=T){
    print("+ Ensuring last track shows genomic units...",v=verbose)
    i <- length(TRKS)
    unit_divisor <- if(guess_genomic_units(gg = TRKS[[i]])=="Mb") 1 else 1000000
    TRKS[[i]] <- suppressMessages(
        TRKS[[i]] + theme(axis.text.x = element_text(),
                          axis.title.x = element_text()) +
            labs(x="Mb") +
            scale_x_continuous(labels = function(x)x/unit_divisor)
    )
    return(TRKS)
}