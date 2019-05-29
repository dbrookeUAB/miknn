library(miknn)
test <- data.frame(
  group = c(rep("a",1000),rep("b", 1000), rep("c",1000), rep(letters[4:10],1000)),
  random =  runif(10000, min = 0, max = 1e5),
  grp_a = c(rnorm(1000,mean = 10000, sd = 1000 ),runif(9000, min = 0, max = 1e5)),
  grp_alone = c(rnorm(1000,mean = 10000, sd = 1000 ),runif(9000, min = 30000, max = 1e5)),
  grp_ab = c(rnorm(1000,mean = 10000, sd = 1000 ),
             rnorm(1000,mean = 10000, sd = 1000 ),
             runif(8000, min = 0, max = 1e5)),
  grp_a_b = c(rnorm(1000,mean = 10000, sd = 1000 ),
              rnorm(1000,mean = 80000, sd = 1000 ),
              runif(8000, min = 0, max = 1e5)),
  grp_abc = c(rnorm(1000,mean = 10000, sd = 1000 ),
              rnorm(1000,mean = 10000, sd = 1000 ),
              rnorm(1000,mean = 10000, sd = 1000 ),
              runif(7000, min = 0, max = 1e5)),
  grp_mid = c(rnorm(1000,mean = 50000, sd = 1000 ),
              runif(9000, min = 0, max = 1e5))
  
  )
mi_knn_fast(test, "group","random", k =10)

