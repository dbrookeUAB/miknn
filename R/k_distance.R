k_distance <- function(x,k,start) {
    
  x[,3] %>%
      dist(method = "maximum") %>%  
      as.matrix() %>%               
      as.data.frame() %>%          
      .[, start] %>%                    
      sort() %>%                   
      .[k+1]
}