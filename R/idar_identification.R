

## * func: identification
## ** doc
#' Check if causal effect if identifiable
#'
#' Check if the causal effect is identifiable given the
#' cause, effect, and the DAG
#'
#' @param dag    a string with the DAG
#' 
#' @param cause  a string with the name of the node
#'               to check if its causal effect on 'effect'
#'               is identifiable
#' 
#' @param effect a string with the name of the node
#'               to check if the causal effect of 'cause'
#'               on this node is identifiable
#' 
#' @param plot   boolean. If \code{TRUE} plot the DAG and the
#'               identification result
#' 
#' @return
#' It returns a \code{dosearch} object from that package
#'
#' @export
## ** func
identification <- function(cause, effect, dag, plot=FALSE)
{
    check_nodes(cause, effect, dag)

    DAG       = dagitty::dagitty(paste("dag {", dag, "}")) 

    join_dist = dosearch_getdata(DAG) 
    cbn_causal_effect = paste0("p(", effect, "| do(", cause,"))")

    id=dosearch::dosearch(data=join_dist, query=cbn_causal_effect, graph=DAG)

    if (id$identifiable){
        if (plot) {print(plot_identification(cause, effect, dag, T, T))}
    }
    
    return(id)
}

## * Ancillary

identification_reg_formulas <- function(cause, effect, dag)
{
    DAG=dagitty::dagitty(paste("DAG {", dag, "}"))
    ## 
    ## res=identification('X', 'Y', dag, T)
    res=identification(cause=cause, effect=effect, dag=dag, plot=FALSE)
    if (!res$identifiable) {
        string=res
    }else{
        idf=dosearch::get_formula(res)
        indep=unlist(stringr::str_extract_all(idf, pattern="\\|[a-zA-Z1-9,]*\\)"))
        dep=unlist(stringr::str_extract_all(idf, pattern="\\([a-zA-Z1-9,]*\\|"))
        ## 
        if (length(indep)==0) {
            dep=unlist(stringr::str_extract_all(idf, pattern="\\([a-zA-Z1-9,]*"))
            dep=stringr::str_replace(string=dep, pattern="\\(|\\)", replacement="")
            string=glue::glue("{dep} ~ 1")
        }else{
            for (i in 1:length(indep))
            {
                indep[i] = stringr::str_replace_all(string=indep[i],
                                                    pattern="\\||\\)",
                                                    replacement="")
            }
            for (i in 1:length(dep))
            {
                dep[i] = stringr::str_replace_all(string=dep[i],
                                                  pattern="\\||\\(",
                                                  replacement="")
            }
            ## 
            string = c()
            for (i in 1:length(dep))
            {
                if (stringr::str_detect(dep[i], pattern=",")) {
                    depis = stringr::str_split(string=dep[i], pattern=",", simplify=TRUE)
                    for (depi in depis)
                    {
                        string = c(string, glue::glue("{depi} ~ {indep[i]}"))
                    }
                }else{
                    string = c(string, glue::glue("{dep[i]} ~ {indep[i]}"))
                }
            }
            for (i in i:length(string))
            {
                string[i]=stringr::str_replace_all(string=string[i], pattern=",", replacement=" + ")
            }
        }
    }
    return(string)
}

identification_paths <- function(cause, effect, dag)
{
    formulas = identification_reg_formulas(cause, effect, dag)
    structure = get_structure(formulas)

    paths = structure %>% dplyr::pull(paths) %>% paste0(., collapse = ' \n ')
    paths = dagitty::paths(dagitty::dagitty(paste("dag {", paths, "}")), cause, effect, directed = TRUE)
    paths = paths$paths
    ## 
    path_direct = glue::glue("{cause} -> {effect}")
    if ((!include_cause_coef(cause, effect, dag)) & path_direct %in% paths) {
        paths = paths[paths != path_direct]
    }
    return(paths)
}

identification_reg_formula_path_effects <- function(cause, effect, dag)
{
    formulas = identification_reg_formulas(cause, effect, dag)
    structure = get_structure(formulas)

    ## paths    = structure %>% dplyr::pull(paths) %>% paste0(., collapse = ' \n ')
    ## paths_id = paths(dagitty(paste("dag {", paths, "}")), cause, effect, directed = TRUE)
    ## paths    = paths_id$paths

    ## 
    ## paths     = structure %>% dplyr::pull(paths) %>% paste0(., collapse = ' \n ')
    ## paths_id  = paths(dagitty(paste("dag {", paths, "}")), 'X', 'Y', directed = TRUE)
    ## paths_dag = paths(dagitty(paste("dag {", dag, "}")), 'X', 'Y', directed = TRUE)
    ## paths     = intersect(paths_id$paths, paths_dag$paths)
    ## 
    ## paths  = structure %>% dplyr::pull(paths) %>% paste0(., collapse = ' \n ')
    ## paths  = paths(dagitty(paste("dag {", paths, "}")), 'X', 'Y', directed = TRUE)$paths
    ## 
    paths=identification_paths(cause, effect, dag)
    path_effects=c()
    path_number=1
    for (path in paths)
    {
        nodes = c(stringr::str_split(string=path, pattern=" -> ", simplify=TRUE))
        ## 
        path_effects[path_number]=glue::glue("Path_causal_effect{path_number} := ") 
        for(i in 1:(length(nodes)-1)) {
            idx = structure$name==nodes[i] & structure$to==nodes[i+1]
            ##
            coef=structure[idx,'coef']
            if (i==1) {
                path_effects[path_number] = glue::glue("{path_effects[path_number]} {coef}") 
            }else{
                path_effects[path_number] = glue::glue("{path_effects[path_number]}*{coef}") 
            }
        }
        path_number=path_number+1
    }
    total_effect=paste("Total_effect := ",
                       paste0(stringr::str_replace(
                                           string=path_effects,
                                           pattern="^.*:= *",
                                           replacement=""),
                              collapse = ' + ')
                       )
    mod_string=(
        structure
        %>% dplyr::pull(mod)
        %>% paste0(., collapse = " \n ")
        ## 
        %>% paste0(., ' \n', collapse = " \n ")
        %>% paste(., paste(path_effects, collapse = ' \n '), collapse = " \n ")
        ## 
        %>% paste0(., ' \n', collapse = " \n ")
        %>% paste(., total_effect, collapse = " \n ")
        %>% paste0(., ' \n', collapse = " \n ")
    )
    return(mod_string)
}

get_total_effect_coef <- function(cause, effect, dag)
{
    total_effect = (
        identification_reg_formula_path_effects(cause, effect, dag)
        %>% stringr::str_split(string=., pattern="\n", simplify=TRUE)
        %>% stringr::str_trim(.)
        %>% c(.)
    )
    idx=stringr::str_detect(total_effect , pattern="Total_effect")
    total_effect = total_effect[idx]
    total_effect = (
        c(stringr::str_split(string=total_effect, pattern=":=", simplify=TRUE))
        %>% stringr::str_trim(.)
    )
    return(total_effect[2])
}

dosearch_getdata <- function(DAG)
{
    lat   = dagitty::latents(DAG)
    nodes = names(DAG)
    ## nodes = unique(c(dagitty::edges(DAG)$v,
    ##                  dagitty::edges(DAG)$w))
    obs   = setdiff(nodes, lat)
    obs   = paste0(obs, collapse = ", ")
    p = paste0("p(", obs, ")")
    return(p)
}

get_id_reg_formula <- function(cause, effect, dag)
{
    idscm = identification_reg_formulas(cause, effect, dag)
    idscm = get_structure(idscm)$formulas  %>% unique(.)
    return(idscm)
}

get_id_scm <- function(cause, effect, dag)
{
    idscm = (
        get_id_reg_formula(cause, effect, dag)
        %>% stringr::str_replace_all(string=., pattern="~", replacement="=") 
    )
    return(idscm)
}
