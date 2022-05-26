
## * func: simulateDAGdata
## ** doc
#' Simulate data
#'
#' Simulate data from the DAG
#'
#' @inheritParams identification
#' 
#' @param n       sample size
#' @param seed    number to set the seed 
#' 
#'  
#' @export
## ** func
simulateDAGdata <- function(dag, n=500, seed=NULL)
{
    if (!is.null(seed)) {
        set.seed(seed)
    }
    DAG = dagitty::dagitty(paste("dag {", dag, "}"))
    df=dagitty::simulateSEM(DAG, N=n) %>% tibble::as_tibble() 
    return(df)
}
