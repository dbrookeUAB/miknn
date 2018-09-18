## Subset Nearest Neighbors 
mi_knn_v2.0 <-function(df, var.c,var.d,id, k){
  df <- df[ordered(df[[id]]), ]
  # df <- df[!duplicated(df[[id]]), ]
  df <- df[ordered(df[[var.c]]), ]
  df <- df %>% arrange_at(c(var.d,var.c))
  name_list <- unique(df[[var.d]]) %>% sort()
  temp = vector()
  
  for (j in name_list) {
   
    z = df[df[[var.d]]==j,]
    N_x = length(z[[var.c]])
    distance = vector(length = N_x)
    for (i in 1:N_x) {
      if (i - k < 1) {
        start = 1
        end = i  + k
        position = k                       
      } else if (i  <= (N_x - k)) {
        start = i  - k
        end = i  + k
        position = k + 1                                 
      }  else {
        start =  i  - k
        end =  N_x
        position = k + 1                                 
      }
      
      temp_vec <- as.numeric(z[[var.c]][start:end]) %>% sort 
      distance[i] = knn_dist(temp_vec, position)[k+1]  
    }
    
    temp = c(temp, distance)
    
  }
 df <- cbind(df,distance = temp)
 df <- df %>% arrange_at(var.c)
 df$m <- neighbors(df[[var.c]], df$distance)
 df <- df %>% group_by_at(var.d) %>% add_tally() %>% rename(N_x = n) %>% ungroup()
 return(df)
}
 
  
