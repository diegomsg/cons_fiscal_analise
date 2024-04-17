# Funções ----
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modulus <- function(x) {
  if_else(
    sign(x) == -1,
    -x,
    x
  )
}

em_numero <- function(x) {
  x |>
    stringr::str_remove("[R\\$]") |>
    stringr::str_remove_all("[\\.|\\)]") |>
    stringr::str_replace_all("\\(", "-") |>
    stringr::str_replace_all(",", "\\.") |>
    as.numeric()
}

em_real <- function(x, dec = 2) {
  em_numero(x) |>
    formatC(digits = dec, format = "f", 
            big.mark = ".", decimal.mark = ",") |>
    paste("R$", ... = _)
}

Nweekdays <- Vectorize(
  function(a, b) {
    return(sum(!(
      ((as.numeric(b:a)) %% 7) %in% c(2,3)
    )) - 1) # 2: Saturday and 3: Sunday
  })

tempo_banco <- Vectorize(function(a, b) {
  while(wday(a) %in% c(7, 1)) { # if weekday, get next until monday
    a <- a + 1
  }
  b - a
})

inicio_mes <- Vectorize(function(x) {
  stopifnot(lubridate::is.Date(x))
  y <- lubridate::year(x) %>% as.numeric()
  m <- lubridate::month(x) %>% as.numeric()
  lubridate::make_date(year = y, month = m, day = 1L) %>%
    lubridate::as_date()
})

valida_cnpj <- function(cnpj) {
  cnpj <- gsub("[^[:digit:]]", "", cnpj)
  if(cnpj == '') return(FALSE)
  
  if (nchar(cnpj) != 14)
    return(FALSE)
  
  # Elimina CNPJs invalidos conhecidos
  invalidos <- c(
    "00000000000000", "11111111111111", "22222222222222", "33333333333333",
    "44444444444444", "55555555555555", "66666666666666", "77777777777777", 
    "88888888888888", "99999999999999"
  )
  if(cnpj %in% invalidos) return(FALSE)
  
  # Valida DVs
  tamanho <- nchar(cnpj) - 2
  numeros <- substr(cnpj, 1, tamanho)
  digitos <- substr(cnpj, tamanho + 1, nchar(cnpj))
  soma <- 0
  pos <- tamanho - 7
  for (i in tamanho:1) {
    soma <- soma + as.numeric(substr(numeros, tamanho - i + 1, tamanho - i + 1)) * pos
    pos <- pos - 1
    if (pos < 2)
      pos <- 9
  }
  resultado <- ifelse(soma %% 11 < 2, 0, 11 - soma %% 11)
  if (resultado != as.numeric(substr(digitos, 1, 1)))
    return(FALSE)
  
  tamanho <- tamanho + 1
  numeros <- substr(cnpj, 1, tamanho)
  soma <- 0
  pos <- tamanho - 7
  for (i in tamanho:1) {
    soma <- soma + as.numeric(substr(numeros, tamanho - i + 1, tamanho - i + 1)) * pos
    pos <- pos - 1
    if (pos < 2)
      pos <- 9
  }
  resultado <- ifelse(soma %% 11 < 2, 0, 11 - soma %% 11)
  if (resultado != as.numeric(substr(digitos, 2, 2)))
    return(FALSE)          
  return(TRUE)  
}
