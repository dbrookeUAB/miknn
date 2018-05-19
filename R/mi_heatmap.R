mi_heatmap<- function(df){
  require(dplyr)
  require(superheat)
  
  df %>% map_df(class) %>% gather() %>% count(value) -> test
  
  if(test$n[test$value=="character"]>1) stop("character columns > 1. Need only one for row labels.")
  
  df %>% select_if(is.numeric) ->temp
  
  
  
  tmp_plot<-superheat(temp,
            row.dendrogram = TRUE, 
            col.dendrogram = TRUE,
            bottom.label.text.angle = 90, 
            bottom.label.size = 0.3,
            bottom.label.text.size = 3 ,          
            grid.hline = FALSE, 
            grid.vline = FALSE,
            clustering.method = "hierarchical"
            ,pretty.order.cols = TRUE, 
            pretty.order.rows = TRUE)
  
  return(tmp_plot)
} 