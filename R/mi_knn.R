#' Mutual Information Calculation (fast)
#'
#' @param df
#' @param k
#'
#' @return
#' @export
#'
#' @examples
mi_knn <- function(df, k, var.c, var.d, id) {
  require(stringr)
  require(dplyr)
  require(purrr)
  require(miknn)
  Rcpp::sourceCpp('src/knn_dist.cpp')

  # Tidy Data ---------------------------------------------------------------

  df <- df[order(df[[var.c]]), ]
  df <-  df[!duplicated(df[[id]]), ]
  df <- df[!is.na(df[[var.c]]), ]

  # Define Variables for Mutual Information Calculation ---------------------

  N = length(df[[var.c]])
  k = as.integer(k)

  # Storage Vectors ---------------------------------------------------------

  ### I = digamma(N)- digamm(N_x)+digamma(k)- digamma(m)

  distance = vector(length = N)        # Distance from its Kth neighbor

  df2 <- df %>%
    inner_join(df %>%
                 group_by_at(var.d) %>%
                 mutate(N_x = n()) %>%
                 group_by_at(c(var.d,"N_x")) %>%
                 summarise_at(vars(var.c), .funs = c(c_x = n_distinct)), by = var.d)
  
  N_x <- df2$N_x


  # For Loop Calculation ----------------------------------------------------

  for (i in 1:N) {
    var_filter <- df[[var.d]][i]
    id_i <- df[[id]][i]
    
    # create a project specific dataframe (169 us)
    z <- subset(df, df[[var.d]] == var_filter)
    
    
    # Position in the subset table (17.2 us)
    subset.position <- match(id_i, z[[id]]) 

    ## Subset Nearest Neighbors
    if (subset.position - k < 1) {
      start = 1
      end = subset.position  + k
      position = subset.position
    } else if (subset.position  <= (N_x[i] - k)) {
      start = subset.position  - k
      end = subset.position  + k
      position = k + 1
    }  else {
      start =  subset.position  - k
      end =  N_x[i]
      position = k + 1
    }

    ## Subset Distance Calculation
    temp_vec <- as.numeric(z[[var.c]][start:end])
    distance[i] = knn_dist(temp_vec, k)[k + 1]

  }
  
  m <- neighbors(df[[var.c]],distance)
  
  # Binding Results ---------------------------------------------------------
 
  info <- cbind(df2,  distance, m)

 
  return(info)
}
