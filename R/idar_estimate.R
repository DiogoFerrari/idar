
## * func: 
## ** doc
#' Estimate DAG model
#'
#' Estimate the causal effect based on the DAG model
#'
#' @inheritParams identification
#' 
#' @param  data a data.frame with the data
#'
#' @param categorical a string vector with the name of the
#'                    categorical endogenous variables
#'                   (see lavaan documentation for 'ordered'
#'                    variables)
#' 
#' @param ordinal    a string vector with the name of the
#'                   ordinal discrete endogenous variables
#'                   (see lavaan documentation for 'ordered'
#'                    variables)
#'
#' @param weights    string with the name of the variable in the
#'                   data frame with the sample weights. Optional.
#' 
#'  
#' @export
## ** func
estimate <- function(cause, effect, dag, data,
                     categorical=NULL,
                     ordinal=NULL,
                     weights=NULL)
{
    check_nodes(cause, effect, dag)

    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 
    id=identification(cause=cause, effect=effect, dag=dag)

    if (id$identifiable) {
        
        mod_string = identification_reg_formula_path_effects(cause, effect, dag)

        ind  = dagitty::localTests(DAG, data=data)
        fit  = lavaan::sem(mod_string,
                           data=data,
                           ordered=c(categorical, ordinal),
                           sampling.weights = weights
                           )

        res=list(fit=fit,
                 local_independencies=ind,
                 cause=cause,
                 effect=effect,
                 dag=dag,
                 model=mod_string
                 )
        class(res) = 'ida'
    }else{
        res='Not-identifiable'
        cat('\n')
        cat(res)
        cat('\n')
    }
    return(res)
}
