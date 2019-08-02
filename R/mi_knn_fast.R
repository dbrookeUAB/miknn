#' Faster Mutual Information Calculation (experimental)
#'
#' @param df a data.frame object
#' @param var.d  the name of the discrete variable in quotations 
#' @param var.c  the name of the continuouse variable in quotations
#' @param k  the number of neighbors for MI calculation. 
#' @param output  set to 'raw' to get the I calculated for each point, or 'global' for globa MI. 
#' @import data.table
#' @return
#' @export
#'
#' @examples
mi_knn.fast <- function(df, var.d, var.c, k, output = "default") {
  # create copy of df and coerce into data.table (median=95.041 us)
  DT <- as.data.table(df)
  
  setkeyv(DT, var.c)                        # sort by var.c  (median = 845.683 us)
  DT[ ,N_x:= length(get(var.c)), var.d]     # size of each group (median = 822.13 us)
  DT[,distance:=.kVector(get(var.c), k) ,var.d]    # calculates distance in k window (median = 9.761301 ms)
  DT$m <- .neighbors(DT[[var.c]],DT[['distance']]) 
  
  n<-length(DT[[var.c]])
  
  if(!output %in% c("raw", "global")){
    # calculates I for each discrete variable (median = 2.337137 ms)
    result<-DT[,.(I = digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))), var.d]
    setorderv(result, var.d)
    return(result)
  } else if(output=="global"){
    # calculates I for each discrete variable (median = 1.986791 ms)
    result<-DT[,digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))]
    return(result)
  } else {
    DT[,I:=digamma(n)-digamma(N_x)+digamma(k)-digamma(m)][]
    attr(DT,"k") <- k
    return(DT)
  }
}