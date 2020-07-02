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
#' @param debug  experimental
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
mi_knn.mod <- function(dt, var.d, var.c, k = NULL, 
                   warnings = TRUE, FORCE = TRUE, global = TRUE, quite = FALSE, debug = FALSE) {
  df <- data.table::as.data.table( data.table::copy(dt))
  data.table::setkeyv(df, var.c)
  
  # subsetting and parameter determination ----------------------------------
  num_zeros <- sum(df[[var.c]] == 0)
  
  if(is.null(k)){
    k <- .kmax(df, var.d, var.c)    # determine the highest possible k
    if(!quite){message(paste("Using k =", k))}}
  
  n <- length(df[[var.d]])                  # used later for calculating I 
  df[ ,N_x:= length(get(var.c)), var.d]     # adds a column for group size
  
  # errors and warning messages ---------------------------------------------
  
  # checking if k was set to a value larger than the smallest group size
  if (nrow(df[, .(N_x = unique(N_x)), var.d][N_x < k]) != 0 & !FORCE) {
    stop("group smaller than k: decrease k, consider setting k = NULL, or set FORCE = TRUE", call. = FALSE)}

  # distance to kth nearest neighbor ----------------------------------------
  if (num_zeros > 0 ) {
    df[,type:=data.table::fifelse(get(var.c)==0, 'd','c')]
    df[type=='d',m:=.N]
    df[type=='d',ki:=.N,var.d]
    df[type=='c',ki:=k]
    df[type=='c',distance:= .kVector(get(var.c), k), var.d]
    df[type=='c', m:=.neighbors(get(var.c), distance)]
    df[, I:=digamma(n)- digamma(N_x)+digamma(ki)-digamma(m)]
    df[type=='c', I:=digamma(n-num_zeros)- digamma(N_x)+digamma(ki)-digamma(m)]
  } else {
    df[,distance:= .kVector(get(var.c), k), var.d]
    df[, m:=.neighbors(get(var.c), distance)]
    df[, I:=digamma(n)- digamma(N_x)+digamma(k)-digamma(m)]

    }
  
  # mutual information calculation ------------------------------------------
  if(!global){
    df_final <-
      df[, .(I = mean(I)), var.d]
    setkeyv(df_final, var.d)
    class(df_final) <- c("specific_mi",class(df_final))
  } else {
    df_final <- df[,  mean(I)]
    class(df_final) <- "global_mi"
  }
  base::attr(df_final,"k") <- k
  if(debug){
    return(df)
  } else(
    return(df_final)
  )
}


