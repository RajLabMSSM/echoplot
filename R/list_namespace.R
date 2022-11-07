#' List namespace
#' 
#' List all objects (e.g. functions, data) 
#' in the namespace one or more R packages.
#' @param packages A character vector of package names.
#' @param status_filter Filter by namespace status (e.g. "export","import").
#' If \code{NULL} (default), will return all namespace objects.
#' @param verbose Print messages.
#' @returns A \link[data.table]{data.table}.
#' 
#' @export
#' @importFrom data.table := rbindlist setkeyv
#' @importFrom utils installed.packages
#' @examples 
#' nm_dt <- list_namespace(packages=c("data.table","pals"))
list_namespace <- function(packages,
                           status_filter=NULL,
                           verbose=TRUE){
    
    fun <- status <- NULL;
    
    #### Validate package names ####
    all_pkgs <- rownames(utils::installed.packages())
    packages < packages[packages %in% all_pkgs]
    if(length(all_pkgs)==0){
        stp <- "0 installed packages found."
        stop(stp)
    }
    messager("Querying",formatC(length(packages)),"packages.",v=verbose)
    #### Get functions ####
    nm_dt <- lapply(packages,
                     function(p){
        messager("Gathering functions from:",p,v=verbose)
        d <- data.table::data.table(
            package=p,
            version=getNamespaceVersion(p),
            fun=ls(asNamespace(p))
        )
        exports <- getNamespaceExports(ns = p)
        imports <- getNamespaceImports(ns = p) 
        d[,status:=ifelse(fun %in% exports,"export",
                          ifelse(fun %in% imports,"import",NA))]
        data.table::setkeyv(d,c("package","status","fun"))
        return(d)
    }) |> data.table::rbindlist()
    if(!is.null(status_filter)){
        nm_dt <- nm_dt[status %in% status_filter,]
    }
    return(nm_dt) 
}
