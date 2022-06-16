#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom sfnetworks activate
## usethis namespace: end
NULL

# Quiet concerns about reserved words nodes and edges from sfnetworks package
if(getRversion() >= "2.15.1")  utils::globalVariables(c("nodes", "edges", "from", "to"))
