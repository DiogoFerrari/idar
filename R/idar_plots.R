
## * Ancillary plots

plot_get_dag_tab <- function(dag)
{
    DAG=dagitty::dagitty(paste("dag {", dag, "}"))
    tab = (
        DAG
        %>% ggdag::tidy_dagitty(.)
        %>% tibble::as_tibble() 
        %>% dplyr::mutate(status = dplyr::case_when(
                                              name %in% dagitty::latents(DAG) ~
                                                  'Unobserved',
                                              T ~ 'Observed'))
    )
    return(tab)
}

plot_idequation <- function(dag, cause, effect, aes_idequation)
{

    size_equation = ifelse(!is.null(aes_idequation$size_equation), aes_idequation$size_equation, 4)
    size_title    = ifelse(!is.null(aes_idequation$size_title), aes_idequation$size_title, 13)
    vjust         = ifelse(!is.null(aes_idequation$vjust), aes_idequation$vjust, .5)
    hjust         = ifelse(!is.null(aes_idequation$hjust), aes_idequation$hjust, .5)
    
    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 

    join_dist = dosearch_getdata(DAG) 
    cbn_causal_effect = paste0("p(", effect, "| do(", cause,"))")
    id=dosearch::dosearch(data=join_dist, query=cbn_causal_effect, graph=DAG)
    ## 
    if (id$formula=='')
    {
        idformula = "Not~identifiable"
    }else
    {
        idformula = id$formula
    }
    idequation = glue::glue("$ {cbn_causal_effect} = {idformula} $") 
    gid  = (
        tibble::tibble(x=0, y=0, label=idequation) 
        %>% ggplot2::ggplot(.) 
        + ggplot2::geom_text(ggplot2::aes(x=x, y=y, colour="black"),
                             label=list(latex2exp::TeX(idequation,
                                                       output = "character")),
                             colour="black", show.legend = FALSE, parse=TRUE,
                             vjust=vjust,
                             hjust=vjust,
                             position=ggplot2::position_dodge(width=0),
                             lineheight = 0.8,
                             family="sans",
                             ## fontface='normal',
                             angle=0, size=size_equation)  
        + ggplot2::labs(
                       x        = NULL,
                       y        = NULL,
                       color    = NULL, 
                       fill     = NULL,
                       linetype = NULL,
                       shape    = NULL,
                       title    = '  Identification result',
                       subtitle = NULL,
                       caption  = NULL
                   )
        + ggplot2::theme_void()
        + ggplot2::theme(plot.title=ggplot2::element_text(size=size_title,
                                                          face='bold'))
    )
}

plot_scm  <- function(dag, aes_scm)
{

    size_equation = ifelse(!is.null(aes_scm$size_equation), aes_scm$size_equation, 4)
    size_title    = ifelse(!is.null(aes_scm$size_title), aes_scm$size_title, 13)
    vjust         = ifelse(!is.null(aes_scm$vjust), aes_scm$vjust, .8)
    hjust         = ifelse(!is.null(aes_scm$hjust), aes_scm$hjust, .6)
    
    scm=scm2latex(dag)

    g=(
        tibble::tibble(x=0, y=0, label=scm) 
        %>% ggplot2::ggplot(.) 
        + ggplot2::geom_text(ggplot2::aes(x=x, y=y, colour="black"),
                             label=list(latex2exp::TeX(scm,
                                                       output = "character")),
                             parse=TRUE,
                             colour="black", show.legend = FALSE, 
                             vjust=vjust,
                             hjust=hjust,
                             position=ggplot2::position_dodge(width=0),
                             lineheight = 0.8,
                             family="sans",
                             ## fontface='normal',
                            angle=0, size=size_equation)  
    + ggplot2::labs(
                    x        = NULL,
                    y        = NULL,
                    color    = NULL, 
                    fill     = NULL,
                    linetype = NULL,
                    shape    = NULL,
                    title    = 'SCM Linear Equations',
                    subtitle = NULL,
                    caption  = NULL
                )
    + ggplot2::theme_void()
    + ggplot2::theme(plot.title=ggplot2::element_text(size=size_title,
                                                        face='bold'))
)
return(g)
}

plot_scm_ida  <- function(cause, effect, dag, aes_scm_ida)
{
size_equation = ifelse(!is.null(aes_scm_ida$size_equation), aes_scm_ida$size_equation, 4)
size_title    = ifelse(!is.null(aes_scm_ida$size_title), aes_scm_ida$size_title, 13)
vjust         = ifelse(!is.null(aes_scm_ida$vjust), aes_scm_ida$vjust, 3)
hjust         = ifelse(!is.null(aes_scm_ida$hjust), aes_scm_ida$hjust,.6)

scm=scmid2latex(cause, effect, dag)

total_effect=TRUE
if (total_effect) {
    total_effect=get_total_effect_coef(cause, effect, dag) 
    scm=glue::glue("{scm}\n\nTotal effect: {total_effect}")
}
g=(
    tibble::tibble(x=0, y=0, label=scm) 
    %>% ggplot2::ggplot(.) 
    + ggplot2::geom_text(ggplot2::aes(x=x, y=y, colour="black"),
                            label=list(latex2exp::TeX(scm, output = "character")),
                            colour="black", show.legend = FALSE, parse=TRUE,
                            ## vjust='inward',
                            ## hjust='inward',
                            ## position=ggplot2::position_dodge(width=0),
                            ## ## lineheight = 0.8,
                            vjust=vjust,
                            hjust=hjust,
                            ## position=ggplot2::position_dodge(width=0),
                            lineheight = 0.8,
                            family="sans",
                            ## fontface='normal',
                            angle=0,
                            size=size_equation)  
    ## + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(5, 5)))
    ## + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .5, add=10))
    + ggplot2::labs(
                    x        = NULL,
                    y        = NULL,
                    color    = NULL, 
                    fill     = NULL,
                    linetype = NULL,
                    shape    = NULL,
                    title    = 'Identification Linear Equations',
                    subtitle = NULL,
                    caption  = NULL
                )
    + ggplot2::theme_void()
    + ggplot2::theme(plot.title=ggplot2::element_text(size=size_title,
                                                        face='bold'))
)
return(g)
}

plot_dag_fit <- function(g, tab=NULL, dag, fit, aes_fit)
{
    ## 
    edge_label_color_pos = 'blue'
    edge_label_color_neg = 'red'
    ## 
    stars            = ifelse(!is.null(aes_fit$stars), aes_fit$stars, TRUE)
    ci               = ifelse(!is.null(aes_fit$ci), aes_fit$ci, TRUE)
    digits           = ifelse(!is.null(aes_fit$digits), aes_fit$digits, 4)
    ## 
    edge_label_pos   = ifelse(!is.null(aes_fit$pos), aes_fit$pos, .7)
    edge_label_face  = ifelse(!is.null(aes_fit$face), aes_fit$face, 'bold')
    edge_label_size  = ifelse(!is.null(aes_fit$size), aes_fit$size, 4)
    edge_label_alpha = ifelse(!is.null(aes_fit$alpha), aes_fit$alpha, .95)
    edge_label_color = ifelse(!is.null(aes_fit$color), aes_fit$color, FALSE)
    edge_label_greys = ifelse(!is.null(aes_fit$greys), aes_fit$greys, FALSE)
    if (!edge_label_color ) {edge_label_color =NULL}
    ##
    total_effect_size = ifelse(!is.null(aes_fit$total_effect_size), aes_fit$total_effect_size, 10)
    total_effect_hjust = ifelse(!is.null(aes_fit$total_effect_hjust), aes_fit$total_effect_hjust, 0)
    total_effect_vjust = ifelse(!is.null(aes_fit$total_effect_vjust), aes_fit$total_effect_vjust, 0)
    ## 
    ## 
    DAG = dagitty::dagitty(paste("dag {", dag, "}"))
    ## 
    est = tidysummary(fit)
    ## 
    if (is.null(tab)) {tab = plot_get_dag_tab(dag)}
    ## 
    tab = (
        tab
        %>% tidyr::unite(name, direction, to, col=path , sep="", remove=FALSE)
        %>% dplyr::full_join(., est , by=c("path")) 
        %>% dplyr::mutate(
                       edge_label = round(est, digits=digits),
                       x_label = (   edge_label_pos*x +
                                     (1-edge_label_pos)*xend),
                       y_label = (   edge_label_pos*y +
                                     (1-edge_label_pos)*yend),
                       status = dplyr::case_when(
                                           name %in% dagitty::latents(DAG) ~
                                               'Unobserved',
                                           T ~ 'Observed'),
                       edge_label_color = dplyr::case_when(
                                                     is.null(edge_label_color) &
                                                     est>=0 ~
                                                         edge_label_color_pos,
                                                     is.null(edge_label_color) &
                                                     est< 0 ~
                                                         edge_label_color_neg,
                                                     )
                   ) 
        %>% dplyr::rowwise(.)
        %>% dplyr::mutate(stars   = dplyr::case_when(
                                               pvalue<.001~"***",
                                               pvalue<.01~"**",
                                               pvalue<.05~"*",
                                               pvalue<.1~"^",
                                               TRUE~''),
                          )
    )
    ## 
    if (edge_label_greys & is.null(edge_label_color))
    {
        tab = (
            tab
            %>% dplyr::mutate(
                           edge_label_color =
                               dplyr::case_when(
                                          edge_label_color==
                                          edge_label_color_pos ~
                                              'black',
                                          edge_label_color==
                                          edge_label_color_neg ~
                                              'grey',
                                          )
                       )
        )
    }
    if (stars)
    {
        tab=(
            tab
            %>% dplyr::rowwise(.)
            %>% dplyr::mutate(
                           edge_label=
                               dplyr::case_when(
                                          !is.na(est)~
                                              glue::glue("{edge_label}{stars}"),
                                          TRUE~''
                                      )
                       ) 
            %>% dplyr::ungroup(.) 
        )
    }
    if(ci)
    {
        tab=(
            tab
            %>% dplyr::rowwise(.)
            %>% dplyr::mutate(edge_label=glue::glue("{edge_label}\n({round(ci.lower, digits=digits)},{round(ci.upper, digits=digits)})
") ) 
            %>% dplyr::ungroup(.) 
        )
    }
    ##
    g = g + (
        ggplot2::geom_label(data=tab %>% dplyr::filter(!is.na(est)),
                            ggplot2::aes(x=x_label,
                                         y=y_label,
                                         label=edge_label),
                            show.legend = FALSE,
                            parse=FALSE,
                            ## label.padding = grid::unit(0, "lines"),
                            label.size = 0,
                            ## ## 
                            ## vjust=1.5,
                            ## hjust=0.5,
                            ## position=ggplot2::position_dodge(width=0),
                            ## family="sans",
                            ## angle=0,
                            ## 
                            colour=(
                                tab
                                %>% dplyr::filter(!is.na(est))
                                %>% dplyr::pull(edge_label_color)
                            ),
                            alpha=edge_label_alpha,
                            fontface=edge_label_face,
                            size=edge_label_size
                            )
    )
    ## Total effect
    total_effect = NULL
    total_effect = tab %>% dplyr::filter(term=='Total effect')  %>% dplyr::pull(edge_label)
    if (!is.null(total_effect)) {
        total_effect = stringr::str_replace(string=total_effect , pattern="\\n", replacement=" ")
        total_effect = glue::glue("Total effect: {total_effect}" )
        g = (
            g
            + ggplot2::labs(caption = total_effect)
            + ggplot2::theme(plot.caption = ggplot2::element_text(size = total_effect_size,
                                                                  hjust = total_effect_hjust,
                                                                  vjust = total_effect_vjust,
                                                                  face = "bold")
                             )
        )
    }
    return(g)
}


## * func: plot_dag
## ** doc
#' Plot the DAG
#'
#' @inheritParams identification
#'
#' @inheritParams plot_identification
#'
#' @param show_legend boolean. If \code{TRUE}, show legend
#'                    with meaning of the nodes aesthetics.
#' 
#' @return
#' It returns a ggplot object with the DAG
#' 
#'  
#' @export
## ** func
plot_dag <- function(dag, fit=NULL, aes_fit=list(), show_legend=FALSE)
{
DAG=dagitty::dagitty(paste("dag {", dag, "}"))
tab = plot_get_dag_tab(dag)
## 
g =  (
    tab
    %>% ggplot2::ggplot(ggplot2::aes(
            x = x, 
            y = y, 
            xend = xend, 
            yend = yend, 
            )) +
        ggdag::geom_dag_edges() + 
        ggdag::geom_dag_point(ggplot2::aes(color=status), shape=1) +
        ggdag::geom_dag_text(col = "black") +
        ggdag::theme_dag() + 
        ggdag::scale_adjusted() +
        ggdag::expand_plot(expand_y = ggplot2::expansion(c(0.2, 0.2))) +
        ggplot2::scale_color_manual(values=c('Unobserved'='grey', 'Observed'='black')) +
        ggplot2::labs(
                        x        = NULL,
                        y        = NULL,
                        color    = NULL, 
                        fill     = NULL,
                        linetype = NULL,
                        shape    = NULL,
                        title    = NULL,
                        subtitle = NULL,
                        caption  = NULL
                    )+
        ggplot2::theme(
                        legend.direction = "horizontal",
                        legend.position = c(0, 1),
                        legend.justification = c(0,1),
                        )
) 

if (!is.null(fit)) {
    g = plot_dag_fit(g, tab, dag, fit, aes_fit)
}

if (!show_legend) {
    g = g + ggplot2::theme(legend.position = "none") 
}
return(g)
}


## * func: plot_identification 
## ** doc
#' Plot DAG and identification result
#'
#' Plot the DAG and identification result computed using
#' the \code{identification} function
#'
#' @inheritParams identification
#'
#' @param scm     boolean, if \code{TRUE} plot the SCM
#'                using a linear model for the
#'                conditional dependencies
#' 
#' @param scm_ida boolean, if \code{TRUE} plot the
#'                identification analysis equations
#'                using linear models
#'
#' @param fit     output of the function \code{estimate}. If provided
#'                it plots the total effect and the coefficiets of the
#'                SCM alongside the edges of the DAG
#'                
#'
#' @param size_equation    numeric, font size of the expression in the
#'                         identification expression subplot
#' 
#' @param size_title       numeric, font size of the title of the
#'                         identification expression subplot
#' 
#' @param aes_scm          a named list with the aesthetic parameters for the
#'                         SCM equations plot. See \code{Details}.
#' 
#' @param aes_scm_ida      a named list with the aesthetic parameters for the
#'                         SCM equations derived from the identification analysis
#'                         See \code{Details}.
#' 
#' @param aes_idequation   a named list with the aesthetic parameters for the
#'                         identification expression. See \code{Details}.
#' 
#' @param aes_fit          a named list with the aesthetic parameters for the
#'                         identification expression. See \code{Details}.
#' 
#' @details
#' The named list aes_* contains the aesthetic parameters of different subplots.
#' The names must match the parameter label.
#'
#' @export
## ** func
plot_identification <- function(cause, effect, dag,
                                scm=FALSE, scm_ida=FALSE, fit=NULL,
                                ## 
                                size_equation=4,
                                size_title=13,
                                ## 
                                aes_scm=list(),
                                aes_scm_ida=list(),
                                aes_idequation=list(),
                                aes_fit=list()
                                ## 
                                )
{
    ## 
    gdag = plot_dag(dag)
    gid  = plot_idequation(dag, cause, effect, aes_idequation)
    if (!is.null(fit)) {
        gdag = plot_dag_fit(gdag, tab=NULL, dag, fit, aes_fit)
    }
    if (!scm & !scm_ida) {
        g=gdag /  gid  + patchwork::plot_layout(heights = c(4,1))
    }
    if (scm & !scm_ida) {
        gscm=plot_scm(dag, aes_scm)
        layout="AAB\nAAB\nAAB\nDDD"
        g=patchwork::wrap_plots(A=gdag, B=gscm, D=gid, design = layout)
    }
    if ((!scm) & scm_ida) {
        gscmida=plot_scm_ida(cause, effect, dag, aes_scm_ida)
        layout="AAB\nAAB\nAAB\nDDD"
        g=patchwork::wrap_plots(A=gdag, B=gscmida, D=gid, design = layout)
    }
    if (scm & scm_ida) {
        gscm=plot_scm(dag, aes_scm)
        gscmida=plot_scm_ida(cause, effect, dag, aes_scm_ida)
        layout=" AAB\nAAC\nDDD "
        g=patchwork::wrap_plots(A=gdag, B=gscm, C=gscmida, D=gid, design = layout)
        ## g=(gdag + (gscm / gscmida)) / gid  +
        ##     patchwork::plot_layout(design = layout)
    }

    ## g=ggpubr::ggarrange(plotlist=plotlist,
    ##                     nrow=2,  heights = c(4,1))
    ## g=ggpubr::ggarrange(plotlist=plotlist)
    return(g)
}

## * func: equivalent DAGs
## ** doc
#' Brief description
#'
#' Long description
#'
#' @inheritParams identification
#' 
#'  
#' @export
## ** func
plot_dag_equivalence <- function(dag)
{
    DAG=dagitty::dagitty(paste("dag {", dag, "}")) 

    dageq = dagitty::equivalentDAGs(DAG)
    gs = list()
    i=1
    for (dageqi in dageq)
    {
        gs[[i]] = plot_dag(dageqi)
        i=i+1
    }
    g = ggpubr::ggarrange(plotlist=gs)
    print(g)
    invisible(g)
}
## * =====================
## * func: default method
## ** doc
#' Plot results of the SCM estimation
#'
#' @inheritParams print.ida
#' 
#' @inheritParams summary.ida
#'
#' @inheritParams plot_identification
#'  
#' @export
## ** func
plot.ida <- function(x, aes_fit=list(), ...)
{
    g=plot_dag(x$dag, fit, aes_fit=aes_fit)
    return(g)
}
