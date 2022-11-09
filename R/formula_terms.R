formula_terms <- function(formula_str){  
    valid_terms <- trimws(strsplit(formula_str,"[.]|[~]|[+]|[*]")[[1]])
    valid_terms <- unique(valid_terms[valid_terms!=""])
    return(valid_terms)
}