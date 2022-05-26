

get_structure <- function(formulas, coef='b')
{
    structure = (
        formulas
        %>% lavaan::lavaanify(.)
        %>% tibble::as_tibble() 
        %>% dplyr::mutate(coef=paste0(coef, 1:nrow(.)),
                          name=rhs,
                          direction = dplyr::case_when(op=='~'~'->'),
                          to=lhs,
                          ) 
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(paths=glue::glue("{name} {direction} {to}"),
                          mod  =glue::glue("{to} ~ {coef}*{name}") ) 
        %>% tidyr::drop_na(direction) 
        ## scm=(
        ##     structure
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(terms = glue::glue("{coef}*{name}")) 
        %>% dplyr::group_by(to)
        %>% dplyr::mutate(formulas = glue::glue("{to[1]} ~ {paste(terms, collapse=' + ')}")) 
    ) 
    return(structure)
}

get_dag_reg_formula <- function(dag)
{
    formulas  = dag2lavaan(dag)
    formulas  = c(stringr::str_split(string=formulas, pattern=" \n ", simplify=TRUE))
    structure = get_structure(formulas, coef='a') 
    formulas  = unique(structure$formulas)
    return(formulas)
}

scm2latex <- function(dag)
{
    scm=get_dag_scm(dag) 
    scm=paste(scm, collapse =' \n ')
    scm=glue::glue(" {scm} ")
    return(scm)
}

get_dag_scm <- function(dag)
{
    DAG = dagitty::dagitty(paste("dag {", dag, "}")) 

    latents = dagitty::latents(DAG)

    scm = get_dag_reg_formula(dag)
    scm = stringr::str_replace_all(string=scm, pattern="~", replacement="=") 

    for (latent in latents)
    {
        scm = stringr::str_replace_all(
                           string=scm,
                           pattern=glue::glue("...{latent}"),
                           replacement=glue::glue("({latent})")
                           ) 
    }
    return(scm)
}

dag2lavaan <- function(dag)
{
    DAG = dagitty::dagitty(paste("dag {", dag, "}")) 

    edges = dagitty::edges(DAG)
    edges = edges %>% dplyr::rename('child'='w',
                                    'parent'='v',
                                    'edge'='e') 
    edges = (
        edges
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(lavaan_syntax = dplyr::case_when(
                                                     edge == '->' ~ "~",
                                                     parent %in% dagitty::exogenousVariables(DAG) | 
                                                     child  %in% dagitty::exogenousVariables(DAG) | 
                                                     is.ordered(child)   ~ "",
                                                     TRUE ~ "~~"
                                                 ),
                          lavaan_syntax = paste (child, lavaan_syntax, parent) 
                          )
    )
    res=paste0(edges$lavaan_syntax, collapse = ' \n ')
    return(res)
}

scmid2latex <- function(cause, effect, dag)
{
## ** 
    scm=get_id_scm(cause, effect, dag) 
    scm=paste(scm, collapse =' \n ')
    scm=glue::glue(" {scm} ")
    return(scm)
}

include_cause_coef <- function(cause, effect, dag)
{
    if(cause_has_direct_effect(cause, effect, dag)){
        include_cause_coef_from_outmodel = TRUE
    }else{
        if(is_cause_adjustment(cause, effect, dag))
        {
            include_cause_coef_from_outmodel = FALSE
        }else{
            include_cause_coef_from_outmodel = TRUE
        }
    }
    return(include_cause_coef_from_outmodel )
}

is_cause_adjustment <- function(cause, effect, dag)
{
    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 

    formulas=identification_reg_formulas(cause, effect, dag)

    structure = get_structure(formulas)
    outmod_covars = (
        structure
        %>% dplyr::filter(to==!!effect & name!=!!cause) 
        %>% dplyr::pull(name)
    )
    cause_is_adjustment=FALSE
    for (outmod_covar in outmod_covars)
    {
        adjSet = unlist(dagitty::adjustmentSets(DAG, outmod_covar, effect, 'minimal', 'total'))
        if (cause %in% adjSet) {
            cause_is_adjustment=TRUE
        }
    }
    return(cause_is_adjustment)
}

cause_has_direct_effect <- function(cause, effect, dag)
{
    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 

    paths_directed = dagitty::paths(DAG, cause, effect, directed=TRUE)
    path_direct = glue::glue("{cause} -> {effect}")

    if (path_direct %in% paths_directed) {
        return(TRUE)
    }else{
        return(FALSE)
    }
}

check_nodes <- function(cause, effect, dag)
{
    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 
    if ( !cause %in% names(DAG)) {
        stop(glue::glue("{cause} not in the DAG"))
    }
    if ( !effect %in% names(DAG) ) {
        stop(glue::glue("{effect} not in the DAG"))
    }
}
