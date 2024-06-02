# tidyxl/unpivotr approach to tidy acordos


# dependencias ------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(unpivotr)
require(tidyr)
require(purrr)


# parametros --------------------------------------------------------------

br_locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")


# funcoes -----------------------------------------------------------------

load_acordo_file <- function(file) {
  xlsx_cells(file) |>
    filter(!is_blank) |>
    select(row:character, -is_blank)
}

valida_xlsx_cells <- function(tbl) {
  #valida 
  col_names <- c("row", "col", "content", "data_type", "error", "logical", "numeric", "date", "character")
  names(tbl) |>
    setdiff(col_names) |>
    length() == 0L
}

partition_acordos <- function(data) {
  # particiona cada arquivo de acordo em blocos de acordo individial
  stopifnot(
    "must be data frame" = is.data.frame(data),
    "must be valid" = valida_xlsx_cells(data)
    )
  l1_corners_text <- c(stringr::regex("Acordo \\d+"))
  l1_corners <- filter(data, stringr::str_detect(character, l1_corners_text))
  partition(data, l1_corners) |>
    select(acordo = character, acordo_full_data = cells)
}

partition_acordos_l2 <- function(data) {
  # particiona cada conjunto de acordo nos 4 blocos
  stopifnot(
    "must be data frame" = is.data.frame(data),
    "must be valid" = valida_xlsx_cells(data)
  )

  l2_corners_text <- c("Detalhes", "Cobranças originais", "Parcelas do acordo")
  l2_corners <- filter(data, character %in% l2_corners_text)
  
  partition(data, l2_corners) |>
    select(name = character, cells)
}

check_block <- function(data, type) {
  data$character[1] == type
}

tidy_detalhes <- function(cells) {
  # arruma bloco detalhes
  # 1 linha de saída
  stopifnot(
    "not valid block" = check_block(cells, "Detalhes")
  )
  
  cells |>
    slice(-1) |>
    behead(direction = "left", tipo) |> 
    select(tipo, character) |> 
    pivot_wider(names_from = tipo, values_from = character) |>
    janitor::clean_names() |>
    mutate(cod_acordo = as.integer(cod_acordo),
           efetuado_em = lubridate::dmy(efetuado_em))
}

list_pick <- function(list, pos) {
  list[[pos]]
}

tidy_det_acordos <- function(blocos) {
  # arruma bloco detalhes a partir de blocos em nested list of tbl
  # 1 tbl = 1 line
  pull(blocos, cells) |>
    list_pick(1) |>
    tidy_detalhes()
}

tidy_cobrancas <- function(cells) {
  # arruma bloco cobrancas
  # 1 linha de saída
  stopifnot(
    "not valid block" = check_block(cells, "Cobranças originais")
  )
  
  acrescimos <- cells |>
    slice_tail(n = 4) |>
    behead(direction = "left", header) |>
    select(-(row:content)) |>
    spatter(header) |> 
    janitor::clean_names() |>
    select(acrescimos, total_devido) |>
    mutate(across(everything(), ~readr::parse_number(.x, locale = br_locale)))
  
  cells |>
    slice(-1) |>
    slice(1:(n()-5)) |> 
    behead(direction = "up", header) |> 
    select(row, data_type, numeric, character, header) |>
    spatter(header) |> 
    select(-row) |>
    janitor::clean_names() |>
    filter(!is.na(composicao)) |>
    mutate(numero = as.double(numero),
           competencia = lubridate::my(competencia),
           composicao = readr::parse_number(composicao, locale = br_locale),
           vencimento = lubridate::dmy(vencimento)) |>
    fill(everything()) |> 
    nest(cobrancas = everything()) |>
    bind_cols(acrescimos)
}

tidy_cob_acordos <- function(blocos) {
  # arruma bloco cobranças a partir de blocos em nested list of tbl
  # 1 tbl = 1 line
  pull(blocos, cells) |>
    list_pick(2) |>
    tidy_cobrancas()
}

tidy_parcelas <- function(cellls) {
  # arruma bloco parcelas
  # 1 linha de saída, ou múltiplas nested
  stopifnot(
    "not valid block" = check_block(cellls, "Parcelas do acordo")
  )
  lastrow <- max(cellls$row)
  cellls |>
    slice(-1) |>
    filter(row < lastrow) |> 
    behead(direction = "up", header) |> 
    select(row, data_type, numeric, character, header) |>
    spatter(header) |> 
    select(-row) |>
    janitor::clean_names() |>
    mutate(across(c(liquidacao, vencimento), lubridate::dmy),
           across(c(emitido, pago), ~readr::parse_number(.x, locale = br_locale))) |>
    relocate(numero, vencimento, emitido, observacao, liquidacao, pago) |>
    nest(parcelas = everything())
}

tidy_parc_acordos <- function(blocos) {
  # arruma bloco parcelas a partir de blocos em nested list of tbl
  # 1 tbl = 1 line
  pull(blocos, cells) |>
    list_pick(3) |>
    tidy_parcelas()
}

acordos_read_folder <- function(folder) {
  stopifnot(
    "must be folder" = fs::is_dir(folder)
  )
  # read folder
  fs::path(folder) |>
  # read files
    fs::dir_ls(glob = "*.xlsx") |>
    tibble(files = _,
           content = map(files, load_acordo_file),
           blocos = map(content, partition_acordos)) |>
    unnest(blocos) |>
    select(-files, -content) |>
    mutate(blocos = map(acordo_full_data, partition_acordos_l2)) |>
    select(-acordo_full_data) |>
    mutate(detalhes = map(blocos, tidy_det_acordos),
           cobrancas = map(blocos, tidy_cob_acordos),
           parcelas = map(blocos, tidy_parc_acordos)) |> 
    select(-blocos) |>
    unnest(c(detalhes, cobrancas, parcelas)) |>
    mutate(cobrado = map_dbl(cobrancas, ~with(.x, sum(composicao)))) |>
    relocate(cobrado, .before = acrescimos) |>
    mutate(total_emitido = map_dbl(parcelas, ~sum(.x$emitido)),
           total_pago = map_dbl(parcelas, ~sum(.x$pago) |> 
                                  coalesce(0)),
           quitado = total_pago >= total_emitido,
           vencido = map_vec(parcelas, ~max(.x$vencimento) <= Sys.Date()) & !quitado)
}
