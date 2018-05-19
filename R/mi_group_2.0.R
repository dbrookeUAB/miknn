#' Calculating Mutual Information for Discrete Variables
#'
#' @param df
#' @param k
#'
#' @return
#' @export
#'
#' @examples
mi_group_2.0 <- function(df, k,  var_d){
  require(dplyr)
  require(rlang)
  df <- as_tibble(df)
  
  var_d <- enquo(var_d)
  
  N <- dim(df)[1]
  
  
  df %>% 
    group_by(!! var_d) %>%
    summarise("I" = {
      digamma(N) - mean(digamma(N_x)) + digamma(k)  - mean(digamma(m))})
}