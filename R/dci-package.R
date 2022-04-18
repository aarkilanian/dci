#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom sfnetworks activate
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the dplyr tidy selection
## To be replaced with explicit calls in concerning functions in new version
if(getRversion() >= "2.15.1")  utils::globalVariables(c("edges",
                                                        "nodes",
                                                        "from",
                                                        "to",
                                                        "weight",
                                                        "member.label",
                                                        "node.label",
                                                        "segweight",
                                                        "type",
                                                        "from_len",
                                                        "to_len",
                                                        "degree",
                                                        "rivID",
                                                        "complexID",
                                                        "rowID",
                                                        "n",
                                                        "component",
                                                        "perm",
                                                        "id",
                                                        "geometry",
                                                        "group"))
