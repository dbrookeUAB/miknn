#' Calculating Mutual Information for Discrete Variables
#'
#' @param df
#' @param k
#'
#' @return
#' @export
#'
#' @examples
mi_group <- function(df, k,  var.d){
  require(dplyr)
  N <- length(df[[var.d]])
  
  df %>% group_by_at(vars(one_of(var.d))) %>%
    summarise("I" = {
      digamma(N) -mean(digamma(N_x)) + digamma(k)  - mean(digamma(m))})
}