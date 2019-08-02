#' Mutual Information Calculation
#'
#' @param dt a data.frame object
#' @param var.d  the name of the discrete variable in quotations 
#' @param var.c  the name of the continuouse variable in quotations
#' @param k  the number of neighbors for MI calculation. 
#' @param warnings  set to FALSE to hide warnings
#' @param FORCE  set to TRUE continues despite not using an optimum k
#' @param global set to FALSE to get specific MI
#' @param quite  set to TRUE to prevent messages being displayed to the console
#' @import data.table
#' @importFrom Rcpp evalCpp
#' @useDynLib miknn, .registration=TRUE
#' @return
#' @export
#'
#' @examples
#' library(miknn)
#' 
#' #TCGA data
#' data("gene_exp",package = "miknn")
#' gene_exp
#' 
#' # basic example using k = 10
#' mi_knn(dt = gene_exp, var.d = "project", k = 10, var.c = "CCND1")
#' 
#' # the function can decide the optimum k
#' mi_knn(dt = gene_exp, var.d = "project", var.c = "CCND1")
#' 
#' # set global = FALSE to get the specific MI for each levels,  
#' mi_knn(dt = gene_exp, var.d = "project", var.c = "CCND1", global = FALSE)
#' 
#' # generating a plot for specific MI
#' res <- mi_knn(dt = gene_exp, var.d = "project", var.c = "CCND1", global = FALSE)
#' plot(res)
#' 
mi_knn <- function(dt, var.d, var.c, k = NULL, 
                    warnings = TRUE, FORCE = TRUE, global = TRUE, quite = FALSE) {
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
    class(df_final) <- c("specific_mi",class(df_final))
  } else {
    df_final <- df[,  digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))]
    class(df_final) <- "global_mi"
  }
  base::attr(df_final,"k") <- k
  
  return(df_final)
}

#' print.global_mi
#'
#' @param x an mi result
#' @param ... For the "function" method of plot, ... can include any of the other arguments of curve, except expr.
#'
#' @return
#' @export
#'
#' @examples
print.global_mi <- function(x,...){
  base::attributes(x) <- NULL
  base::print(x,...)
}
  
#' plot.specific_mi
#'
#' @param x  a result from mi_knn(dt, var.d,var.d, global = FALSE)
#' @param ... For the "function" method of plot, ... can include any of the other arguments of curve, except expr.
#'
#' @return
#' @export
#'
#' @examples
plot.specific_mi <- function(x,...){
  graphics::par(mar=c(5,8,4,2)) # increase y-axis margin.
  graphics::par(las=2)
  x<- x[base::order(x[[1]])]
  graphics::barplot(x[,I],names  = x[[1]],
          cex.names=0.8, ylab = "Mutual Information (bits)",...)
}
  
  
