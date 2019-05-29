#' Mutual Information Calculation (fast)
#'
#' @param k
#' @param var.c
#' @param DT 
#' @param warnings 
#' @param var.d
#'
#' @return
#' @export
#'
#' @examples
mi_knn_fast <- function(df, var.d, var.c, k, output = "default") {
  # create copy of df and coerce into data.table (median=95.041 us)
  DT <- copy(df)
  setDT(DT)
  
  # sort by var.d then var.c  (median = 845.683 us)
  setkeyv(DT, cols = c(var.d,var.c))
  
  result<-DT[,sapply(.SD, function(x) .kVector(x,k)), by = var.d, .SDcols = var.c]
  setnames(result, "V1","distance")
  result$var.c <- DT[[var.c]]
  setkey(result, var.c)
  result$m<-result[, .neighbors(var.c, distance)]
  
  n<-length(result$var.c)
  
  if(!output %in% c("raw", "global")){
    result<-result[,.(I=digamma(n)-digamma(length(m))+digamma(k)-mean(digamma(m))), by = var.d]
    setkeyv(result, var.d)
    return(result)
  } else if(output=="global"){
    result<-result[,mean(digamma(n))-digamma(length(m))+digamma(k)-mean(digamma(m))]
    return(result)
  } else {
    result<-result[,.(I=digamma(n)-digamma(length(m))+digamma(k)-digamma(m)), by = var.d]
    result <-result[,mean(I)]
    return(result)
  }
  
  
  
  
}