#' Determine the Optimum K
#'
#' @param dt a data.frame object
#' @param var.d  the name of the discrete variable in quotations 
#' @param var.c  the name of the continuouse variable in quotations
#' @param k.series a vector of integers to test
#' @param global  set as TRUE by default. Set to FALSE for specific MI
#'
#' @return
#' @export
#' @import progress
#'
#' @examples
#' library(miknn)
#' 
#' # determining I for a series of k
#' mi_kopt(iris, "Species","Sepal.Length", global = FALSE)
#' 
mi_kopt <- function(dt, var.d, var.c,  global = TRUE, returnK = FALSE){
  suppressWarnings({
    
    k.series <- c(1:dt[,.N, var.d][which.min(N),N-1])
    pb <- progress::progress_bar$new(
      format = ":percent k = :k [:bar] :elapsed | eta: :eta",
      total = length(k.series),    # 100 
      width = 60, clear = TRUE
    )
    if(global){
      result <- data.table(k = k.series, 
                 I = sapply(k.series, function(x){
                   pb$tick(tokens = list(k = x))
                   mi_knn(dt, var.d,var.c, x, global = TRUE, FORCE = TRUE)
                 })
      )[!is.na(I)]
      result$se <- stats::predict(stats::loess(I~k, result, span = 0.5), se = TRUE)$se
    } else {
      result <-data.table(k = k.series, 
                 do.call(rbind,
                         lapply(k.series, function(x) {
                           pb$tick(tokens = list(k = x))
                           dcast(
                             mi_knn(dt, var.d, var.c, k = x, FORCE = TRUE, global = FALSE), as.formula(paste0(".~",var.d)), value.var = "I")[,-1]
                         }
                         )
                 )
      )
    } 
  })
  class(result) <- c("mi_kopt",class(result))
  
  if(!returnK&global){
    return(result)
  } else {
    result[,I/sqrt(se),k][order(V1, decreasing = TRUE),k][1]
  }
  
}


#' Plot mi_kopt
#'
#' @param x result from mi_kopt
#' @param ... For the "function" method of plot, ... can include any of the other arguments of curve, except expr.
#' 
#' @import data.table
#' @return
#' @export
#'
#' @examples
plot.mi_kopt <- function(x,...){
  model <- stats::predict(stats::loess(I~k, x, span = 0.5), se = TRUE)
  
  ci <- data.frame(
    k = x$k,
    fit = model$fit,
    upper =model$fit + qt(0.995,model$df)*model$se,
    lower = model$fit - qt(0.995,model$df)*model$se
    )
  
  ylim <- c(min(ci$lower)-min(ci$lower)*0.025, max(ci$upper)+max(ci$upper)*0.025)
  
  graphics::plot(I~k, x,pch = 19,ylim = ylim, ...)
  
  graphics::lines(fit~k,ci, col = "black", lty = 1)
  graphics::lines(upper~k, ci, col = "red", lty = 4)
  graphics::lines(lower~k, ci, col = "red", lty = 4)

}
