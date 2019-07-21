#' Mutual Information Calculation
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
mi_knn <- function(dt, var.d, var.c, k = NULL, 
                    warnings = TRUE, FORCE = FALSE, global = FALSE, quite = FALSE) {
  df <- as.data.table(copy(dt))
  setkeyv(df, var.c)
  
  # subsetting and parameter determination ----------------------------------
  num_zeros <- sum(df[[var.c]] == 0)
  
  if(is.null(k)){
    k <- .kmax(df, var.d, var.c)    # determine the highest possible k
    if(!quite){message(paste("Using k =", k))}}
  
  if(num_zeros>0){
    remove_zero <- df[[var.c]] != 0    # creates a T/F vector for subsetting the data.table
    df <- df[remove_zero,]}             # subsets data.table 
  n <- length(df[[var.d]])                  # used later for calculating I 
  df[ ,N_x:= length(get(var.c)), var.d]     # adds a column for group size
  
  # errors and warning messages ---------------------------------------------
  
  # checking if k was set to a value larger than the smallest group size
  if (nrow(df[, .(N_x = unique(N_x)), var.d][N_x < k]) != 0 & !FORCE) { 
    stop("group smaller than k: decrease k, consider setting k = NULL, or set FORCE = TRUE", call. = FALSE)}
  
  # warning message for the number of data points equal to 0
  if (num_zeros > 0 & warnings) {
    num_not_zeros <- sum(dt[[var.c]] != 0)
    warning(paste(num_zeros, "points in the vector named", var.c,
                  "have 0 as their value. Running calculation on the remaining",
                  num_not_zeros,"data points"))}
  
  # distance to kth nearest neighbor ----------------------------------------
  df[,distance:= .kVector(get(var.c), k), var.d]
  df$m <- .neighbors(df[[var.c]], df[['distance']])
  
  # mutual information calculation ------------------------------------------
  if(!global){
    df_final <-
      df[, .(I = digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))), var.d]
  } else {
    df_final <- df[,  digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))]
  }
  
  return(df_final)
}