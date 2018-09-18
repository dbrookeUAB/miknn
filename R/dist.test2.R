mut_info_kNN <- function(df, k = 3) {
  
  distance = vector(length = length(df$HTSeq__FPKM_UQ))
  N_x = vector(length = length(df$HTSeq__FPKM_UQ))
  m = vector(length = length(df$HTSeq__FPKM_UQ))
  I = vector(length = length(df$HTSeq__FPKM_UQ))
  
  ## k_distance function for calculating kth nearest neighbor distance within subsets
  k_distance <- function(x) {
    suppressWarnings({
      x %>%
        dist(method = "maximum") %>%
        as.matrix() %>%
        as.data.frame() %>%
        .[, 4] %>%
        sort() %>%
        .[4]
    })
  }
  
  N = length(df$HTSeq__FPKM_UQ)
  digamma_N = digamma(N)
  digamma_k = digamma(k)
  
  for (i in 1:length(df$HTSeq__FPKM_UQ)) {
    # Subset based dataframe based on the value of this row's project_short_name
    z <- df[df$project_short_name == paste0(df[i, 1]), ]
    
    # Position in the subset table
    subset.position <-
      str_which(z$sample_barcode, paste0(df[i, 2]))
    
    if (subset.position - k < 1) {
      start = 1
      end = start + k
    } else if (subset.position < (length(z$sample_barcode) - (2 * k + 1))) {
      start = subset.position - k
      end = subset.position + k
    }  else {
      start =  length(z$sample_barcode) - (2 * k + 1)
      end =  length(z$sample_barcode)
    }
    z[start:end, ] %>% k_distance -> distance[i]
    
    df[df$HTSeq__FPKM_UQ >= (df$HTSeq__FPKM_UQ[i] - distance[i]) & df$HTSeq__FPKM_UQ <= (df$HTSeq__FPKM_UQ[i] + distance[i]), ]$x %>%
      length() -> N_x[i]
    m[i] <- N_x[i]
    
    I[i] = digamma_N - digamma(N_x[i] + digamma_k - digamma(m[i]))
  }
  result <- cbind(df,m,distance,I)
  return(result)
}
