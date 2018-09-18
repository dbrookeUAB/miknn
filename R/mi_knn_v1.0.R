#' Mutual Information Calculation (fast)
#'
#' @param df
#' @param k
#'
#' @return
#' @export
#'
#' @examples
mi_knn_v1.0 <- function(df, k, var.c, var.d, id) {
  require(stringr)
  require(dplyr)
  require(purrr)
  require(miknn)
  Rcpp::sourceCpp("src/knn_dist.cpp")
  df <- df[ordered(df[[var.c]]), ]

  df <- df[!is.na(df[[var.c]]), ]
  
  N = length(df[[var.c]])
  k = as.integer(k)
  distance = vector(length = N)
  m= vector(length = N)
  df2 <- df %>% inner_join(df %>% group_by_at(var.d) %>% mutate(N_x = n()) %>% 
                             group_by_at(c(var.d, "N_x")) %>% summarise_at(vars(var.c), 
                                                                           .funs = c(c_x = n_distinct)), by = var.d)
  N_x <- df2$N_x
  for (i in 1:N) {
    var_filter <- df[[var.d]][i]
    id_i <- df[[id]][i]
    z <- subset(df, df[[var.d]] == var_filter)
    subset.position <- match(id_i, z[[id]])
    if (subset.position - k < 1) {
      start = 1
      end = subset.position + k
      position = subset.position
    }
    else if (subset.position <= (N_x[i] - k)) {
      start = subset.position - k
      end = subset.position + k
      position = k + 1
    }
    else {
      start = subset.position - k
      end = N_x[i]
      position = k + 1
    }
    temp_vec <- as.numeric(z[[var.c]][start:end])
    distance[i] = knn_dist(temp_vec, k)[k + 1]
  }
  df = cbind(df, distance)
  m <- neighbors(df[[var.c]], df$distance)
  info <- cbind(df2, distance, m)
  return(info)
}