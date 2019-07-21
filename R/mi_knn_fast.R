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
mi_knn.fast <- function(df, var.d, var.c, k, output = "default") {
  # create copy of df and coerce into data.table (median=95.041 us)
  DT <- copy(df)
  setDT(DT)
  
  # sort by var.d then var.c  (median = 845.683 us)
  setkeyv(DT, var.c)
  DT[ ,N_x:= length(get(var.c)), var.d]
  DT[,distance:=miknn:::.kVector(get(var.c), k) ,var.d]
  DT[, m:=miknn:::.neighbors(get(var.c), distance)]
  
  n<-length(DT[[var.c]])
  
  if(!output %in% c("raw", "global")){
    result<-DT[,.(I = digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))), var.d]
    setkeyv(result, var.d)
    return(result)
  } else if(output=="global"){
    result<-DT[,digamma(n) - mean(digamma(N_x)) + digamma(k) - mean(digamma(m))]
    return(result)
  } else {
    DT[,I:=digamma(n)-digamma(N_x)+digamma(k)-digamma(m)][]
    return(DT)
  }
}