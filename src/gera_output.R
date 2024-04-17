# Objetivo: gera saída na data para o report
# Author: Diego Guimarães
# Created: 2024-02-20


# dependencias ------------------------------------------------------------



# funcoes -----------------------------------------------------------------

gera_relat <- function(file = "report.html",
                       output_path = "output",
                       output_filename = NULL,
                       date_ref = Sys.Date()) {
  
  date_ref <- stringr::str_replace_all(date_ref, "-", "") |>
    stringr::str_sub(1, 6)
  
  output_filename <- dplyr::coalesce(output_filename, "Rel_cons_fiscal_") |>
      paste0(date_ref, ".html")
      
  fs::file_copy(path = file,
                new_path = paste(output_path, output_filename, sep = "/"),
                overwrite = TRUE)
}


