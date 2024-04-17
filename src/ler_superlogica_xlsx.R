# Objetivo: funções de leitura dos dados
# Origem dos dados: Superlógica
# Author: Diego Guimarães
# Created: 2024-02-17


# bibliotecas -------------------------------------------------------------

require(dplyr)
require(tidyxl)
require(stringr)


# ler dados ---------------------------------------------------------------

get_dados_brutos <- function(x) {
  xlsx_cells(x) %>%
    select(row, col, is_blank, content, data_type, logical,
           numeric, date, character) %>%
    filter(is_blank == FALSE) %>%
    select(!is_blank)
}


# posições dos relatórios -------------------------------------------------

get_posicoes <- function(relat, bruto) {
  relat |>
    mutate(
      matches = map(titulo, \(x) filter(bruto, str_equal(character, x))$row),
      qnt = map_int(matches, length)) |>
    filter(qnt > 0) |>
    mutate(
      row_ini = map_int(matches, min),
      row_end = coalesce(lead(row_ini) -1,
                         max(bruto$row))
    ) |>
    select(-c(cod, matches))
}


# contas ----------------------------------------------------------------
# Relatório 016B = Resumo Financeiro

get_contas <- function(base, posicoes) {
  indice_rel <- 1
  limites <- posicoes[indice_rel,]
  conteudo <- base %>%
    filter(row > limites$row_ini & row <= limites$row_end)
  head <- conteudo %>%
    slice(1:2) %>%
    select(character)

  dados <- conteudo %>%
    slice(-(1:2), -n()) %>%
    rectify() %>%
    select(-1) %>%
    row_to_names(1) %>%
    mutate_at(2:5, em_numero) %>%
    mutate_at(1, toupper)

  result <- tibble(relatorio = head[1,],
                   periodo = head[2,]) %>%
    bind_cols(dados)

  return(result)
}


# saldos ----------------------------------------------------------------
# Relatório 020A = Resumo de saldo

get_saldos <- function(base, posicoes){
  indice_rel <- 2
  limites <- posicoes[indice_rel,]
  conteudo <- base %>%
    filter(row > limites$row_ini & row <= limites$row_end)

  subdados <- conteudo %>%
    slice(-(1:2))

  saldo_index <- filter(subdados, str_detect(character, "^Saldo em \\d*"))$row
  saldo <- filter(subdados, row %in% c(min(saldo_index), max(saldo_index))) %>%
    rectify() %>%
    drop_na() %>%
    setNames(c("row", "Data", "Saldo")) %>%
    select(Data, Saldo) %>%
    mutate(Data = map_vec(Data, dmy),
           Saldo = map_dbl(Saldo, em_numero))
  return(saldo)
}


# despesas e resumo de receitas -----------------------------------------------------
# Relatório 020A = Demonstrativo de
# Receitas e Despesas Analítico

get_despesas <- function(base, posicoes){
  relatorio <- base %>%
    slice(3) %>%
    select(character) %>%
    str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
    dmy()
  indice_rel <- 2
  limites <- posicoes[indice_rel,]
  conteudo <- base %>%
    filter(row > limites$row_ini & row <= limites$row_end)

  subdados <- conteudo %>%
    slice(-(1:4))

  subdados_despesas <- subdados %>%
    filter(col != 5)
  despesas_index <- subdados_despesas %>%
    filter(str_detect(character, "^Despesas$|^Total de Despesas$")) %>%
    pull(row)

  despesas_base <- subdados_despesas %>%
    filter(row >= min(despesas_index) & row <= max(despesas_index))

  despesas_grupos <- group_by(despesas_base, row) %>%
    filter(n() == 1) %>%
    rowwise() %>%
    ungroup() %>%
    #filter(row < lead(row)-1) %>% #erro no 2022-11, perdeu último grupo
    select(row, col, Grupo = character)

  despesas_dados <- despesas_base %>%
    behead("up-left", medida) %>%
    behead("left", Despesas) %>%
    behead("left", Liquidação) %>%
    enhead(despesas_grupos, "left-up") %>%
    select(row, data_type, character, medida, Grupo, Despesas, Liquidação) %>%
    spatter(medida) %>%
    select(-row) %>%
    filter(!str_detect(Despesas, "Total de ")) %>%
    rename(Método = `Forma de Pgto.`) %>%
    mutate(Destinatário = str_extract(Despesas, "(([A-Z]|[0-9])+\\s[[A-Z]|[0-9]|\\s|\\.]+[^\\(])*") %>% str_trim(),
           Liquidação = map_vec(Liquidação, ~ifelse(!is.na(dmy(.x)), dmy(.x), my(.x)) %>%
                                  as_date()),
           Obs = map_chr(Despesas, ~str_match(.x, "\\([:print:]*\\)") %>%
                           str_remove_all("\\(|\\)")),
           Relatorio = relatorio,
           Competência = map_vec(Obs, ~str_extract(.x, "\\d{1,2}/\\d{4}") %>% my()),
           Competência = if_else(!is.na(Competência), Competência, as_date(inicio_mes(Liquidação))),
           Parcela = map2_chr(Obs, Despesas,
                              ~coalesce(str_match(.x, "\\d{1,2} de \\d{1,2}[\\s|\\)|$]|\\d{1,2}\\/\\d{1,2}[\\s|\\)|$]"),
                                        str_match(.y, "\\d{1,2} de \\d{1,2}[\\s|\\)|$]|\\d{1,2}\\/\\d{1,2}[\\s|\\)|$]")) %>%
                                str_replace(" de ", "/")) %>% str_trim(),
           Valor = map_dbl(Valor, em_numero),
           Doc_id = map_vec(Documento, ~str_remove_all(.x, "[:punct:]") %>%
                              str_remove_all("\\s") %>%
                              str_extract("\\d+") %>%
                              as.numeric()),
           Parcela_n = map_vec(Parcela, ~str_split_i(.x, "\\/", 1) %>%
                                 as.integer),
           Parcela_tot = map_vec(Parcela, ~str_split_i(.x, "\\/", 2) %>%
                                   as.integer),
           Parcela_pend = Parcela_tot - Parcela_n,
           Saldo_devedor = Parcela_pend * Valor
    )
}


# receitas ----------------------------------------------------------------
# relatório 065A = Receitas
# liquidadas por data de crédito

get_receitas_resumo <- function(base, posicoes){
  relatorio <- base %>%
    slice(3) %>%
    select(character) %>%
    str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
    dmy()
  indice_rel <- 2
  limites <- posicoes[indice_rel,]
  conteudo <- base %>%
    filter(row > limites$row_ini & row <= limites$row_end)

  subdados <- conteudo %>%
    slice(-(1:4))
  subdados_receitas <- subdados %>%
    filter(col != 3)

  receitas_index <- subdados_receitas %>%
    filter(str_detect(character, "^Receitas$|^Total de Receitas$")) %>%
    pull(row)

  receitas_bruto <- subdados_receitas %>%
    filter(row >= min(receitas_index) & row <= max(receitas_index))

  receitas_grupos <- group_by(receitas_bruto, row) %>%
    filter(n() == 1) %>%
    rowwise() %>%
    ungroup() %>%
    filter(row < lead(row)-1) %>%
    select(row, col, Grupo = character)

  receitas_dados <- receitas_bruto %>%
    behead("up-left", medida) %>%
    behead("left", Receitas) %>%
    enhead(receitas_grupos, "left-up") %>%
    select(row, data_type, character, medida, Grupo, Receitas) %>%
    spatter(medida) %>%
    select(-row) %>%
    filter(!str_detect(Receitas, "Total de ")) %>%
    mutate(Competência = map_vec(Competência, my),
           Relatorio = relatorio,
           Valor = map_dbl(Valor, em_numero))
}

get_receitas <- function(base, posicoes){
  relatorio <- base %>%
    slice(3) %>%
    select(character) %>%
    str_extract("\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
    dmy()
  indice_rel <- 4
  limites <- posicoes[indice_rel,]
  conteudo <- base %>%
    filter(row > limites$row_ini+2 & row <= limites$row_end-1)

  receitas <- conteudo %>%
    filter(col != 7) %>%
    behead("up", medida) %>%
    select(row, data_type, character, medida) %>%
    spatter(medida) %>%
    fill(Crédito) %>%
    select(Competência = Compet., Unidade, Data_vencimento = Venc.,
           Data_credito = Crédito, Data_liquidacao = Liquidação,
           Valor_bruto = Pago, Tarifa, Valor_liquido = Creditado) %>%
    drop_na() %>%
    mutate(
      Competência = my(Competência),
      Relatorio = relatorio,
      across(Data_vencimento:Data_liquidacao, dmy),
      across(Valor_bruto:Valor_liquido, em_numero),
      Tempo = tempo_banco(Data_vencimento, Data_liquidacao),
      Atraso = Tempo > 0
    ) %>%
    left_join(unidades, join_by(Unidade == und_name)) %>%
    select(Competência, id, torre, unidade, Data_vencimento:Atraso)
}
