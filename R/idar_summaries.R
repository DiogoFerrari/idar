
## * func: default method print 
## ** doc
#' Print output of fitted model
#'
#' @param x output of the function \code{estimate}
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @export
## ** func
print.ida <- function(x, ...)
{
    fit=x
    digits=4

    est=(
        tidysummary(fit)
        %>% dplyr::select(term, coef, est, se, z,
                          pvalue, dplyr::contains("ci."))
        %>% dplyr::ungroup(.) 
        %>% dplyr::mutate(across(where(is.numeric), ~ round(., digits = digits)))
    )
    paths=(
        tidysummary(fit)
        %>% dplyr::filter(op==':=') 
        %>% dplyr::select(term, path)
        %>% tidyr::unite(term, path, col='note', sep=": ", remove=TRUE)
        %>% dplyr::pull(note)
        %>% paste0(., collapse = '; ')
    )
    ind=fit$local_independencies
    if (nrow(ind)>0) {
        ind = (
            ind
            %>% dplyr::rename('ci.lower'='2.5%',
                              'ci.upper'='97.5%',
                              ) 
            %>% dplyr::mutate(across(where(is.numeric), ~ round(., digits = digits)))
        )
        independencies=TRUE
    }else{
        ind="No independencies implies"
        independencies=FALSE
    }

    cat("\n")
    cat("======================================================")
    cat("\n")
    cat(paste0("Causal effect of ", fit$cause, ' on ', fit$effect))
    cat("\n")
    cat("\n")
    cat("Structural Causal Model (SCM):")
    cat("\n")
    cat(scmid2latex(fit$cause, fit$effect, fit$dag))
    cat("\n")
    cat("\n")
    cat("Estimates:\n")
    cat("\n")
    est %>% as.data.frame %>% print(., digits=digits)
    cat("\n")
    cat("\n")
    cat(paths)
    cat("\n")
    cat("======================================================")
    cat("\n")
    cat("DAG implied local independencies:\n")
    cat("(NULL: Variables are conditionally independent)")
    cat("\n")
    cat("\n")
    if (independencies) {
        print(ind)
    }else{
        cat(ind)
    }
    cat("\n")
    cat("======================================================")
    cat("\n")
}

## * func: default method summary 
## ** doc
#' Summary of SCM estimates
#'
#' @param object output of the function \code{estimate}
#' 
#' @param ... further arguments passed to or from other methods
#' 
#' @inheritParams print.ida
#'  
#' @export
## ** func
summary.ida <- function(object, ...)
{
    lavaan::summary(fit$fit,
                    fit.measures=TRUE,
                    header = TRUE,
                    estimates = FALSE
                    )
    print(object)
    invisible()
}
    


## * func: default method coef 
## ** doc
#' Get coefficients of SCM estimates
#'
#' @inheritParams print.ida
#' 
#' @inheritParams summary.ida
#'
#' 
#' @export
## ** func
coef.ida <- function(object, ...)
{
    return(lavaan::coef(fit$fit))
}
    



## * func: default method predict
## ** doc
#' Generate fitted values from the fitted model
#'
#' @inheritParams print.ida
#' 
#' @inheritParams summary.ida
#'
#' @param type a string with the type of output.
#'             If 'yhat,' generate fitted values
#'             of observed nodes. If 'lv,' generate
#'             fitted values for the latent variables.
#'             See \code{lavaan::lavPredict} documentation.
#' 
#' 
#' @details
#' It generates fitted values using the data used
#' to fit the model. See \code{lavaan::lavPredict} documentation.
#'
#'  
#' @export
predict.ida <- function(object, type='yhat',...)
{
    pred = lavaan::lavPredict(object$fit, type='yhat')
    return(pred)
}


## ** func
## * func: tidysummary
## ** doc
#' Full summary of SCM estimated coefficients
#'
#' @param fit an output of the function \code{estimate}
#' 
#' @return 
#' The function returns a \code{tibble} object with the
#' summary of the estimation.
#' 
#'  
#' @export
## ** func
tidysummary <- function(fit)
{
    est=(
        lavaan::parameterEstimates(fit$fit)
        %>% dplyr::filter(op %in% c(':=', '~')) 
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(
                       cause = dplyr::case_when(op=='~' ~ glue::glue("{rhs}"),
                                                op==':=' ~ fit$cause),
                       effect = dplyr::case_when(op=='~' ~ glue::glue("{lhs}"),
                                                 op==':=' ~ fit$effect),
                       term = dplyr::case_when(op=='~' ~ glue::glue("{lhs}{op}{rhs}"),
                                               op==':=' ~ glue::glue("{lhs}")),
                       coef = dplyr::case_when(op=='~' ~ glue::glue("{label}"),
                                               op==':=' ~ glue::glue("{rhs}")),
                       edge = dplyr::case_when(op=='~' ~ glue::glue("{rhs}->{lhs}"),
                                               op==':=' ~ glue::glue("{rhs}"))
                   ) 
    )
    pattern=est %>% dplyr::filter(op=='~') %>% dplyr::pull(coef)
    replace=est %>% dplyr::filter(op=='~') %>% dplyr::pull(edge)
    for (i in 1:length(pattern))
    {
        est=(
            est
            %>% dplyr::mutate(edge=stringr::str_replace_all(string=edge ,
                                                            pattern=pattern[i],
                                                            replacement=replace[i])) 
        )
    }
    est=(
        est
        %>% dplyr::mutate(
                       edge=stringr::str_replace_all(string=edge ,
                                                     pattern="\\*",
                                                     replacement='->'),
                       ) 
    )
    replace=est %>% dplyr::filter(op=='~') %>% dplyr::pull(lhs)
    pattern=paste0(replace,'->',replace)
    for (i in 1:length(pattern))
    {
        est=(
            est
            %>% dplyr::mutate(
                           edge=stringr::str_replace_all(string=edge ,
                                                         pattern=pattern[i],
                                                         replacement=replace[i]),
                           edge=stringr::str_replace_all(string=edge ,
                                                         pattern=" *\\+ *",
                                                         replacement=' + '),
                           ) 
        )
    }
    est= est %>%
        dplyr::select(term, path=edge, coef, est:ci.upper,
                      dplyr::everything(), -lhs, -rhs) %>% 
        dplyr::mutate_if(is.character, ~stringr::str_replace(string=.,
                                                pattern="Path_causal_effect",
                                                replacement="Path"))  %>% 
        dplyr::mutate_if(is.character, ~stringr::str_replace(string=.,
                                                pattern="Total_effect",
                                                replacement="Total effect")) 
    return(est)
}

## * func: globalfit
## ** doc
#' Global fit measures
#'
#' @inheritParams print.ida
#'
#' @inheritParams summary.ida
#' 
#' @return 
#' Print the global fit measures for the SCM 
#' estimation. See \code{lavaan::fitmeasures}
#' for details
#' 
#' @export
## ** func
globalfit <- function(object,...)
{
    lavaan::summary(object$fit,
                    fit.measures=TRUE,
                    header = FALSE,
                    estimates = FALSE
                    )
    invisible(lavaan::fitmeasures(object$fit))
}

## * func: localfit
## ** doc
#' Local fit measures
#'
#' @inheritParams print.ida
#'
#' @inheritParams summary.ida
#' 
#' @details
#' Print the local fit measures for the DAG
#' and the data set provided. These measures
#' the correlation between variables (nodes) 
#' for each conditional independence implied  
#' by the DAG.
#'
#' A good fit means that we do not reject the
#' hypotheses that the variables are conditionally
#' independent.
#' 
#'  
#' @export
## ** func
localfit <- function(object,...)
{
    ind=object$local_independencies
    ind=data.frame(independence=rownames(ind),
                   ind) %>% 
        dplyr::rename(
                   'ci.lower'='X2.5.',
                   'ci.upper'='X97.5.',
                   ) 
    rownames(ind)=NULL
    cat('\n')
    print(ind)
    invisible(ind)
}

## * func: residuals
## ** doc
#' Return SCM residuals
#'
#' @inheritParams print.ida
#' 
#' @inheritParams summary.ida
#' 
#' @details
#' See \code{lavaan::lavResiduals} for details.
#'  
#' @export
## ** func
residuals <- function(object, ...)
{
    res = lavaan::lavResiduals(object$fit)
    print(res)
    invisible(res)
}
