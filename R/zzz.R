#' @importFrom magrittr %>%
#' @importFrom stats coef

.onAttach<- function(libname, pkgname) 
{
 packageStartupMessage('

 ## idar package

 ')
} 

if(getRversion() >= "2.15.1")  utils::globalVariables(c(
                                          ".",
                                          "across", "child", "ci.upper",
                                          "coef", "direction", "edge",
                                          "edge_label", "fit",
                                          "lavaan_syntax", "lhs", "mod",
                                          "name", "note", "op", "parent",
                                          "path", "pvalue", "rhs", "se",
                                          "status", "term", "to", "where",
                                          "x", "x_label", "xend", "y",
                                          "y_label", "yend", "z"
                                      ))
