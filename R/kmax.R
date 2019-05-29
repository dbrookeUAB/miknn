.kmax <- function(dt, var.d, var.c){
  df <- copy(dt)
  
  kmax<-min(df[get(var.c)!=0,table(get(var.d))])-2
  kmax <- ifelse(kmax>20,20,ifelse(kmax<5,5,kmax))
  return(kmax)
}