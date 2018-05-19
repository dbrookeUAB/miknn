write_query <- function(Ensembl_gene_id){
  require(stringr)
  select_main<- function(Ensembl_gene_id){
    paste0("SELECT\n  project_short_name,\n  aliquot_barcode,\n  ",str_c(Ensembl_gene_id, collapse = ",\n  "))
  }

  select_start<- function(Ensembl_gene_id){
    Ensembl_gene_id <- Ensembl_gene_id[1]
    paste0("\nFROM(\n  SELECT DISTINCT\n    project_short_name,\n    aliquot_barcode,\n    HTSeq__FPKM_UQ AS ",
           Ensembl_gene_id,
           "\n  FROM\n    `rstudio-189018.rnaseq.RNAseq` \n  WHERE\n    Ensembl_gene_id = '",
           Ensembl_gene_id,"')")
  }


  join_add<- function(Ensembl_gene_id){
    end_length <- length(Ensembl_gene_id)
    Ensembl_gene_id <- Ensembl_gene_id[2:end_length]
    paste0("\nJOIN(\n  SELECT DISTINCT\n    project_short_name,\n    aliquot_barcode,\n    HTSeq__FPKM_UQ AS ",
           Ensembl_gene_id,
           "\nFROM\n    `rstudio-189018.rnaseq.RNAseq` \nWHERE\n    Ensembl_gene_id = '",
           Ensembl_gene_id,"')\nUSING(\n    project_short_name,\n    aliquot_barcode)")
  }


  qs_main <- select_main(Ensembl_gene_id)
  qs_start <-select_start(Ensembl_gene_id)
  qs_join <- join_add(Ensembl_gene_id)

  query <- paste0(qs_main, qs_start, str_c(qs_join, collapse = ""))

  return(query)
}

