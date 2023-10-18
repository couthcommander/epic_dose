#' Internal functions
#' @keywords internal
#' @name pk-internal
#' @aliases lu nwn vc
NULL

lu <- function(x) length(unique(x))
nwn <- function(x) suppressWarnings(as.numeric(x))
vc <- EHR:::validateColumns
