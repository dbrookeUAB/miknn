#' Mutual Information Calculation (fast)
#'
#' @param k
#' @param var.c
#' @param dt 
#' @param warnings 
#' @param var.d
#'
#' @return
#' @export
#'
#' @examples
mi_knn_list <- function(dt, var.d, var.c, k = NULL,  warnings = TRUE, FORCE = FALSE, global = FALSE) {
  dt<- as.data.table(dt)
  df <- copy(dt)
  
  # subsetting and parameter determination ----------------------------------
  
  num_zeros <- sum(dt[[var.c]] == 0)
  
  if(is.null(k)){
    k <- .kmax(df, var.d, var.c) -1     # determine the highest possible k
    message(paste("Using k =", k))
  }
  
  if(num_zeros>0){
    remove_zero <- df[[var.c]] != 0    # creates a T/F vector for subsetting the data.table
    df <- df[remove_zero,]             # subsets data.table 
  }
  
  n <- length(df[[var.d]])                  # used later for calculating I 
  
  df[ ,N_x:= length(get(var.c)), var.d]     # adds a column for group size
  df[, m := vector(mode  = "numeric")]     # vector neighbors within x distance
  
  df <-
    df[order(df[[var.c]]), ]       # reorder data based on continuous variable
  name_list <- unique(df[[var.d]])    # list of discrete variables
  
  # errors and warning messages ---------------------------------------------
  
  # checking if k was set to a value larger than the smallest group size
  if (nrow(df[, .(N_x = unique(N_x)), var.d][N_x < k]) != 0 & !FORCE) { 
    stop("group smaller than k: decrease k or consider setting k = NULL", call. = FALSE)
  }
  
  # warning message for the number of data points equal to 0
  if (num_zeros > 0 & warnings) {
    num_not_zeros <- sum(dt[[var.c]] != 0)
    warning(
      paste(
        num_zeros,
        "points in the vector named",
        var.c,
        "have 0 as their value. Running calculation on the remaining",
        num_not_zeros,
        "data points"
      )
    )
  }
  
  # distance to kth nearest neighbor ----------------------------------------
  
  for (j in name_list) {
    z = df[df[[var.d]] == j, ]           # subset the data based on discrete variables
    N_x = length(z[[var.c]])          # length of subset
    distance_temp = vector(length = N_x)   # creates a vector to collect distance measurements
    
    
    for (i in 1:N_x) {
      if (i - k < 1) {
        start = 1
        end = i  + k
        position = i
      } else if (i  <= (N_x - k)) {
        start = i  - k
        end = i  + k
        position = k + 1
      }  else {
        start =  i  - k
        end =  N_x
        position = k + 1
      }
      
      temp_vec <- as.numeric(z[[var.c]][start:end])
      distance_temp[i] = .knn_dist(temp_vec, knn = k, position = position)
    }
    
    temp_subset <- df[[var.d]] == j
    df[temp_subset, distance := distance_temp]
  }
  
  
  # determination of all neighbors within X distance ------------------------
  
  df <- df[order(df[[var.c]]), ]
  df$m <- .neighbors(df[[var.c]], df$distance)
  
  # mutual information calculation ------------------------------------------
  
  
  if(!global){
    df_final <-
      df[, .(I = digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))), var.d]
  } else {
    df_final <- df[,  digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))]
  }
  
  result <- list(group.mi = df_final, k = k, distance = data.table(df$distance), m = data.table(df$m))
  
  return(result)
}