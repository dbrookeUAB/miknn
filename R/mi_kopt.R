#' Determine the Optimum K
#'
#' @param dt 
#' @param var.d 
#' @param var.c 
#' @param k.series 
#' @param global 
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' 
#' # I just think data.tables are better 
#' DT <- as.data.table(iris)
#' 
#' # determining I for a series of k
#' k.series <- 3:40
#' mi_knn_klist(DT, "Species","Sepal.Length", k.series, global = FALSE)
#' 
mi_kopt <- function(dt, var.d, var.c, k.series, global = TRUE){
  suppressWarnings({
    require(progress)
    pb <- progress_bar$new(
      format = ":percent k = :k [:bar] :elapsed | eta: :eta",
      total = length(k.series),    # 100 
      width = 60, clear = TRUE
    )
    
    if(global){
      data.table(k = k.series, 
                 I = sapply(k.series, function(x){
                   pb$tick(tokens = list(k = x))
                   mi_knn(dt, var.d,var.c, x, global = TRUE, FORCE = TRUE)
                 })
      )
    } else {
      data.table(k = k.series, 
                 do.call(rbind,
                         lapply(k.series, function(x) {
                           pb$tick(tokens = list(k = x))
                           dcast(
                             mi_knn(dt, var.d, var.c, k = x, FORCE = TRUE), as.formula(paste0(".~",var.d)), value.var = "I")[,-1]
                         }
                         )
                 )
      )
    } 
  })
}