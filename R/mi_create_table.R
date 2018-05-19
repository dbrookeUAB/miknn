
mi_create_table <- function(path){
  require(dplyr)
  require(tibble)
  require(tidyr)
  require(purrr)

  project_files<-data_frame(
    gene = dir(path = path) %>% str_sub(end=15L),
    filenames = dir(path = path),
    path = paste0(path = path,filenames)
  ) %>% arrange(gene)

  # this function creates the rows from the individual project files
  create_row <- function(path, name){

    # this function transposes the original two-col df to a single row df
    protran<- function(x,y){
      x %>% spread(key = project_short_name, value = I)
    }

    df <- suppressMessages(path %>% read_tsv %>% protran)
    gene <- data_frame(gene = name)
    result<-bind_cols(gene, df)
    return(result)

  }

  result2<- map2_dfr(project_files$path, project_files$gene, create_row)
  return(result2)
}

