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
#' k.series <- 3:40
#' mi_kopt(iris, "Species","Sepal.Length", k.series, global = FALSE)
#' 
mi_kopt <- function(dt, var.d, var.c, k.series, global = TRUE){
  suppressWarnings({
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
  return(result)
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
  model <- stats::predict(stats::loess(I~k, x, span = 0.8), se = TRUE)
  graphics::plot(I~k, x,...)
  
  graphics::lines(model$fit)
  graphics::lines(model$fit - qt(0.995,model$df)*model$se, lty=2)
  graphics::lines(model$fit + qt(0.995,model$df)*model$se, lty=2)
}
