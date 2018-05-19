#' Overall Mutual Information
#'
#' @param df 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
mi_overall <- function(df, k){
  require(dplyr)
  N <- length(df$project_short_name)
  df %>%
    summarise("I" = {
      digamma(N) - mean(digamma(N_x)) + digamma(k)  - mean(digamma(m))})
}
