#' Determine the Optimum K
#'
#' @param dt a data.frame object
#' @param var.d  the name of the discrete variable in quotations 
#' @param var.c  the name of the continuouse variable in quotations
#' @param global  set as TRUE by default. Set to FALSE for specific MI
#' @param returnK  set as TRUE to return the 'optimal' k
#' @param progress whether to show a progress bar (DEFAULT=TRUE)
#' @param k.series the range of integers to scan through (DEFAULT=1:100)
#'
#' @return
#' @export
#' @import progress
#'
#' @examples
#' library(miknn)
#' 
#' # determining I for a series of k
#' mi_kopt(iris, "Species","Sepal.Length", global = TRUE)
#' 
mi_kopt <- function(dt, var.d, var.c,  global = TRUE, returnK = FALSE, progress = TRUE, k.series = NULL){
  suppressWarnings({
    dt <- as.data.table(copy(dt))
    
    if(is.null(k.series)){
      n.test <- dt[,.N,var.d][which.min(N),N]
      if(n.test<100){
        k.series <- c(1:dt[,.N, var.d][which.min(get("N")),get("N")]-1)
      } else {
        k.series <- 1:100
      }
    }
   
    

# function definitions ----------------------------------------------------

    if(progress){
      
      pb <- progress::progress_bar$new(
        format = ":percent k = :k [:bar] :elapsed | eta: :eta",
        total = length(k.series),    # 100 
        width = 60, clear = TRUE
      )
      
      global.p <- function(x){
        pb$tick(tokens = list(k = x))
        mi_knn.fast(dt, var.d,var.c, x, output = 'global')
      }
      
      specific.p <- function(x) {
        pb$tick(tokens = list(k = x))
        dcast(
          mi_knn.fast(dt, var.d, var.c, k = x), as.formula(paste0(".~",var.d)), value.var = "I")[,-1]
      }
    } else {
      global.p <- function(x){
        mi_knn.fast(dt, var.d,var.c, x, output = 'global')
      }
      
      specific.p <- function(x) {
        dcast(mi_knn.fast(dt, var.d, var.c, k = x),
              as.formula(paste0(".~",var.d)), value.var = "I")[,-1]
      }
    }
    
    
 
    if(global){
      result <- data.table(k = k.series, 
                 I = sapply(k.series, global.p)
      )[!is.na(I)]
      result <- cbind(result, as.data.table(stats::predict(stats::loess(I~k, result, span = 0.5), se = TRUE)[1:2]))
    } else {
      result <-data.table(k = k.series, 
                 do.call(rbind,
                         lapply(k.series, specific.p
                         )
                 )
      )
    } 
  })
  class(result) <- c("mi_kopt",class(result))
  
  if(!returnK){
    return(result)
  } else {
    result[,I/sqrt(se.fit),"k"][order(get("V1"), decreasing = TRUE),get("k")][1]
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
