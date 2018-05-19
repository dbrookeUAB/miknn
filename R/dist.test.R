dist.test<-function(df,k){
  x<- vector(length = (length(df$x)-k))
  for(i in 1:k)
    df[1:(k+7),3] %>% 
    dist( diag = TRUE, upper = TRUE, p =3) %>% 
    as.matrix() %>% as.data.frame() %>%
    .[,i] %>% sort() %>% .[4] ->x[i]
  
  for (i in (k+1):(length(df$x)-(k+1))) {
    df[(i-k):(i+k),3] %>% 
      dist( diag = TRUE, upper = TRUE, p =3) %>% 
      as.matrix() %>% as.data.frame() %>%
      .[,4] %>% sort() %>% .[4] -> x[i]
  }
  
  for (i in (length(df$x)-(2*k)):length(df$x)) {
    df[(length(df$x)-(2*k)):(length(df$x)),3] %>% 
      dist( diag = TRUE, upper = TRUE, p =3) %>% 
      as.matrix() %>% as.data.frame() %>%
      .[,i-(length(df$x)-(2*k+1))] %>% sort() %>% .[4] -> x[i]
  }
  return(x)
}
