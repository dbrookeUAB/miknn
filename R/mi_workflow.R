mi_workflow <- function(datasets) {
  require(readr)
  require(dplyr)
  require(progress)

  for (j in datasets) {
    pbc$tick()

    # Import File -------------------------------------------------------------
    print(paste("Importing", j, "dataset", sep = " "))
    imported <- suppressWarnings(suppressMessages(read_csv(paste0("gene_sets/", j), progress = FALSE)))

    # Create Gene List --------------------------------------------------------
    imported %>% select(-project_short_name,-aliquot_barcode) %>% colnames() -> gene_list
    pb <- progress_bar$new(total = length(gene_list))


    # Gene Query Loop ---------------------------------------------------------

    for (i in gene_list) {
      pb$tick()
      print(paste("Calculating mutual information for ", i))
      imported %>% select(project_short_name, aliquot_barcode, i)  -> initial

      colnames(initial) <-
        c("project_short_name",
          "aliquot_barcode",
          "HTSeq__FPKM_UQ")

      temp <-  mi_knn(initial, k = 20)
      write_tsv(temp, path = paste0("info/", i, "_info.txt"))
      temp1 <- mi_projects(temp, k = 20)
      write_tsv(temp1, paste0("project/", i, "_project.txt"))
    }
  }

}
