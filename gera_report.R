# Objetivo: Gerar relatório de contas e salvar na pasta pré-definida
# Author: Diego Guimarães
# Created: 2024-02-25


# dependencias ------------------------------------------------------------

source("src/gera_output.R")


# quarto ------------------------------------------------------------------

report_quarto_file <- "report.qmd"

system(command = paste0("quarto render ", report_quarto_file),
       intern = FALSE,
       wait = TRUE,
       show.output.on.console = TRUE,
       invisible = TRUE,
       timeout = 200)


# exporta versão ----------------------------------------------------------

gera_relat(file = "report.html",
           output_path = "output",
           date_ref = Sys.Date() + lubridate::dmonths(-1))


# apaga temp --------------------------------------------------------------

fs::file_delete("report.html")
