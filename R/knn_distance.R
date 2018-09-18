## Subset Nearest Neighbors 
mi_knn_v2.0 <-function(df, var.c,var.d, k){
  temp = vector()
  df <- df[order(df[[var.d]]),]
  name_list <- unique(df[[var.d]])
  
  for (j in name_list) {
   
    z = df[df[[var.d]]==j,]
    N_x = length(z[[var.c]])
    distance = vector(length = N_x)
    for (i in 1:N_x) {
      if (i - k < 1) {
        start = 1
        end = i  + k
        position = i                       
      } else if (i  <= (N_x - k)) {
        start = i  - k
        end = i  + k
        position = k + 1                                 
      }  else {
        start =  i  - k
        end =  N_x
        position = k + 1                                 
      }
      
      temp_vec <- as.numeric(z[[var.c]][start:end])  
      distance[i] = knn_dist(temp_vec, k)[k + 1]  
    }
    temp = c(temp, distance)
  }
 df <- cbind(df,distance = temp)
 df <- df[order(df[[var.c]]),]
 df$m <- neighbors(df[[var.c]], df$distance)
 
 return(df)
}
 
  
