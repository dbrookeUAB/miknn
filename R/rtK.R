#' Return k used for generating result
#'
#' @param mi a MI result
#'
#' @return
#' @export
#'
#' @examples
rtK <- function(mi){
  attr(mi,"k")
}