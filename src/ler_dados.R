# Objetivo: Ler os dados da prestação de contas condominial em dados lógicos
#           e gerar relatório html para compartilhamento.
# Origem dos dados: Superlógica
# Created: 2023-07-30

# Observações:
## início da conformidade: 2022-12
## motivo: meses anteriores com contas encerradas, não exportáveis


# bibliotecas -------------------------------------------------------------

suppressMessages({
  library(cli)
  library(tidyverse)
  library(janitor)
  library(tidyxl)
  library(unpivotr)
})


# dependências ------------------------------------------------------------

source("src/utils.R")
source("src/ler_superlogica_xlsx.R")
source("src/ler_acordos.R")


# parâmetros --------------------------------------------------------------

config <- config::get(file = "config/config.yml")
conformidade <- as.Date(config$conformidade$data_inicio)


## arquivos brutos xlsx ----------------------------------------------------

arq_xlsx <- config$path$prest_contas |>
  list.files(full.names = TRUE)


## unidades/apartamentos ----------------------------------------------

torres <- str_split(config$unidades$blocos_nomes, ",")[[1]] |>
  stringr::str_squish()

unidades <- expand_grid(ap = 1L:config$unidades$apart_andar,
                        andar = as.integer(1:config$unidades$andares_tipo * 100),
                        torre = torres) |>
  mutate(unidade = ap + andar,
         bloco = map_vec(torre, \(x) which(str_equal(x, torres))),
         id = bloco * 10000 + unidade,
         und_nome = paste0(str_pad(unidade, 4, pad = "0"),
                           "TORRE ",
                           str_to_upper(torre)),
         und_name = paste0(str_pad(unidade, 4, pad = "0"),
                           "TOWER ",
                           str_to_upper(torre)),
         und_abv = paste0(str_pad(unidade, 4, pad = "0"),
                           " ",
                           torre)) |>
  arrange(id)


## empreendimento ----------------------------------------------------------

empreendimento <- list(nome = config$empreendimento$nome,
                       nome_curto = config$empreendimento$nome_curto,
                       nome_completo = config$empreendimento$nome_completo,
                       endereço = config$empreendimento$endereco,
                       num = config$empreendimento$num,
                       cep = config$empreendimento$cep,
                       bairro = config$empreendimento$bairro,
                       cidade = config$empreendimento$cidade,
                       uf = config$empreendimento$uf,
                       cnpj = config$empreendimento$cnpj)


## equipe gestora ----------------------------------------------------------

gestores <- config$administracao$gestores |>
  as_tibble_col() |>
  add_column(eleicao = config$administracao$data_eleicao) |>
  unnest_wider(value)


## relatórios --------------------------------------------------------------

relat_lista <- tibble(cod = c("016B", "020A", "037E", "065A", "020G", "033A",
                              # removido 033A trecho de cheques emitidos, suprimido
                              # no relatorio 2024-02
                              "039A", "060A", "060B")) %>%
    mutate(titulo = paste(cod, empreendimento$nome %>%
                            toupper(), "(70)")
           )


# consolida ---------------------------------------------------------------


## dataframes --------------------------------------------------------------

df_contas <- tibble()
df_saldos <- tibble()
df_despesas <- tibble()
df_receitas_resumo <- tibble()
df_receitas <- tibble()


## cargas ------------------------------------------------------------------

cli_h1("Carregando dados")
cli_alert_info("Localizados {.val {length(arq_xlsx)}} arquivos.")

lin <- 0  # controle de linhas

start <- Sys.time() # controle de tempo

for (i in cli_progress_along(arq_xlsx, name = "Carregando")) {
  # Arquivo na lista
  arq <- arq_xlsx[i]
  cli_alert_info("Lendo {.val {i}} de {.val {length(arq_xlsx)}}: {.val {arq_xlsx[i]}}.")

  # Lê arquivo
  bruto <- get_dados_brutos(arq)

  # Coleta posições de linhas
  pos <- get_posicoes(relat = relat_lista, bruto = bruto)
  cli_alert_success("Índices coletados.")

  # Carrega dados e os une aos dataframes
  contas <- get_contas(base = bruto, posicoes = pos)
  df_contas <- bind_rows(df_contas, contas)
  cli_alert_success("Contas coletadas.")
  lin <- lin + nrow(contas)
  cli_alert_info("{.val {nrow(contas)}} linhas lidas. Total de linhas lidas: {.val {lin}}")
  rm(contas)

  saldos <- get_saldos(base = bruto, posicoes = pos)
  df_saldos <- bind_rows(df_saldos, saldos)
  cli_alert_success("Resumo de saldos lidos.")
  lin <- lin + nrow(saldos)
  cli_alert_info("{.val {nrow(saldos)}} linhas lidas. Total de linhas lidas: {.val {lin}}")
  rm(saldos)

  despesas <- get_despesas(base = bruto, posicoes = pos)
  df_despesas <- bind_rows(df_despesas, despesas)
  cli_alert_success("Despesas lidas.")
  lin <- lin + nrow(despesas)
  cli_alert_info("{.val {nrow(despesas)}} linhas lidas. Total de linhas lidas: {.val {lin}}")
  rm(despesas)

  receitas_resumo <- get_receitas_resumo(base = bruto, posicoes = pos)
  df_receitas_resumo <- bind_rows(df_receitas_resumo, receitas_resumo)
  cli_alert_success("Resumo de receitas lidos.")
  lin <- lin + nrow(receitas_resumo)
  cli_alert_info("{.val {nrow(receitas_resumo)}} linhas lidas. Total de linhas lidas: {.val {lin}}")
  rm(receitas_resumo)

  receitas <- get_receitas(base = bruto, posicoes = pos)
  df_receitas <- bind_rows(df_receitas, receitas)
  cli_alert_success("Receitas detalhadas.")
  lin <- lin + nrow(receitas)
  cli_alert_info("{.val {nrow(receitas)}} linhas lidas. Total de linhas lidas: {.val {lin}}")
  rm(receitas)

  cli_alert_success("Realizada leitura do arquivo {.val {i}} de {.val {length(arq_xlsx)}}.")
}

end <- Sys.time()
tempo <- difftime(end, start) %>% units::as_units("s")

cli_alert_success("Coleta de dados concluída.")
cli_alert_info("Tempo decorrido: {.val {tempo}}")
rm(lin, start, end, tempo)


## ajustes -----------------------------------------------------------------

contas <- df_contas %>%
  select(periodo, Conta, 'Saldo ant.',
         'Créditos*', 'Débitos*', 'Saldo final') %>%
  rename(Competência = periodo, Saldo_inicial = 'Saldo ant.', C = 'Créditos*',
         D = 'Débitos*', Saldo_final = 'Saldo final') %>%
  mutate(
    Competência = sapply(Competência,
                         str_extract,
                         "\\d{1,2}/\\d{1,2}/\\d{2,4}") %>%
      dmy(),
    Conta = str_remove(Conta, "^C/\\s*") %>% as_factor(),
    D = if_else(sign(D) != "-", -D, D),
    Check = Saldo_final - (Saldo_inicial + C + D) < 0.01) %>%
  filter(!str_detect(Conta, "SALDO")) %>%
  filter(Competência >= conformidade)

saldos <- unique(df_saldos) %>%
  mutate(Data = Data + 1) %>%
  filter(Data >= conformidade)

resumo <- contas %>%
  group_by(Competência) %>%
  summarise(Saldo_ini = sum(Saldo_inicial), C = sum(C),
            D = sum(D), Saldo_fim = sum(Saldo_final)) %>%
  filter(Competência >= conformidade)

despesas <- df_despesas %>%
  group_by(Relatorio) %>%
  summarise(Despesas = -sum(Valor)) %>%
  filter(Relatorio >= conformidade)

receitas_gerais <- df_receitas_resumo %>%
  group_by(Relatorio) %>%
  summarise(Receitas = sum(Valor)) %>%
  filter(Relatorio >= conformidade)

receitas_tx_condominio <- df_receitas %>%
  group_by(Relatorio) %>%
  summarise(R_recorrente_total = sum(Valor_bruto),
            Tarifa = sum(Tarifa),
            R_recorrente_liquida = sum(Valor_liquido)) %>%
  filter(Relatorio >= conformidade)


# verificações ------------------------------------------------------------

Check_saldos <- inner_join(resumo,
                           saldos,
                           by = join_by(Competência == Data)) %>%
  mutate(Check_ini = Saldo - Saldo_ini < 0.01,
         Check_fim = Saldo_fim == lead(Saldo_ini))

Check_fluxo <- inner_join(resumo,
                          despesas,
                          by = join_by(Competência == Relatorio)) %>%
  inner_join(receitas_gerais, by = join_by(Competência == Relatorio)) %>%
  inner_join(receitas_tx_condominio, by = join_by(Competência == Relatorio)) %>%
  mutate(R_não_recorrente = Receitas + Tarifa - R_recorrente_liquida,
         Saldo_fim_calc = Saldo_ini + Receitas + Despesas,
         Check = modulus(Saldo_fim - Saldo_fim_calc) < 0.01)


# inadimplência -----------------------------------------------------------

last_competencia <- max(df_receitas$Competência)
meses <- seq.Date(conformidade, last_competencia, by = "month")
Receitas_p_unidade <- expand_grid(id = unidades$id, Competência = meses) %>%
  left_join(df_receitas, by = join_by(id, Competência)) %>%
  mutate(Data_vencimento = if_else(is.na(Data_vencimento),
                                   Competência + days(4),
                                   Data_vencimento),
         Tempo = if_else(is.na(Tempo),
                         difftime(today(), Data_vencimento, "days") %>%
                           as.numeric(),
                         Tempo),
         Atraso = replace_na(Atraso, TRUE),
         Abonada = id %in% gestores$id,
         Quitada = !is.na(Valor_bruto) | Abonada)

Inadimplencia_historica <- Receitas_p_unidade %>%
  group_by(id) %>%
  summarise(Valor_bruto = sum(Valor_bruto, na.rm = T),
            Tarifa = sum(Tarifa, na.rm = T),
            Valor_liquido = sum(Valor_liquido, na.rm = T),
            Tempo_medio = mean(Tempo, na.rm = T),
            Atraso_medio = mean(coalesce(Tempo[Atraso == T], 0)),
            Atraso_max = max(coalesce(Tempo[Atraso == T], 0)),
            Atrasos = coalesce(sum(Atraso, na.rm = T), 0),
            Recorrencia = Atrasos / n(),
            Pri_atraso = first(Competência[Quitada == F]),
            Atrasadas = str_flatten(Competência[Quitada == F], collapse = ", "))

taxa_mensal <- Receitas_p_unidade %>%
  filter(!is.na(Valor_bruto)) %>%
  group_by(Competência) %>%
  summarise(Valor_bruto = getmode(Valor_bruto))

Inadimplencia <- Receitas_p_unidade %>%
  filter(Quitada == F, Competência != last_competencia) %>%
  inner_join(taxa_mensal,
             by = join_by(Competência),
             suffix = c("_ori", "_base")) %>%
  select(id, Competência, torre, unidade, Valor_bruto = Valor_bruto_base,
        Data_vencimento, Tempo) %>%
  mutate(multa = round(.02 * Valor_bruto, 2),
         juros = round((1.09 ^ (Tempo/30) - 1) * Valor_bruto, 2),
         honorários = if_else(Tempo > 60,
                              round(.20 * (Valor_bruto + multa + juros), 2),
                              0),
         Valor_atual = Valor_bruto + multa + juros + honorários)

acordos <- acordos_read_folder(config$path$acordos) |>
  mutate(unidade = str_replace_all(unidade, " TOWER", "TOWER")) |>
  inner_join(unidades, by = join_by(unidade == und_name)) |>
  select(-(ap:und_abv), -unidade, id) |>
  relocate(id, .after = cod_acordo)

acordos_adimplentes <- filter(acordos, !vencido) |>
  select(id, cobrancas) |>
  unnest_wider(cobrancas) |>
  select(id, competencia) |>
  unnest_longer(competencia) |>
  distinct()

acordos_andamento <- filter(acordos, !quitado) |>
  select(id, cod_acordo, cobrancas, parcelas, total_emitido, total_pago, vencido) |>
  select(-c(total_emitido, total_pago)) |>
  mutate(competencias_negociadas = map(cobrancas, ~pull(.x, competencia) |>
                                         unique() |>
                                         as.Date() |>
                                         format("%m/%Y") |>
                                         str_flatten_comma()),
         cobrancas_nao_pagas = map(parcelas, ~pull(filter(.x, is.na(pago)), emitido) |>
                                     sum() |>
                                     unlist()),
         parcelas_a_pagar = map(parcelasm, ~nrow(filter(.x, is.na(pago))))) |>
  select(-(cobrancas:parcelas)) |>
  unnest_longer(c(cobrancas_nao_pagas, competencias_negociadas))

Inadimplencia <- anti_join(Inadimplencia, acordos_adimplentes, by = join_by(id, Competência == competencia))
