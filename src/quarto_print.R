# Funções para imprimir tabela simples e
# tabela paginada a depender do tamanho

library(knitr)

# Format integers
knit_print.integer = function(x, ...) {
  prettyNum(x, big.mark=",")
}

registerS3method(
  "knit_print", "integer", knit_print.integer,
  envir = asNamespace("knitr")
)

# Format double
knit_print.double = function(x, ...) {
  prettyNum(x, decimal.mark = ",", big.mark = ".")
}

registerS3method(
  "knit_print", "double", knit_print.double,
  envir = asNamespace("knitr")
)

# useful function for options
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

# super-customised table printing ----
knit_print.data.frame <- function (x, options, ...) {
  # get options
  digits <- options$digits %||% getOption("digits")
  rownames <- options$rownames %||% FALSE
  pageLength <- options$pageLength %||% 10
  escape <- options$escape %||% TRUE
  caption <- options$table.cap

  # use DT for longer tables in html
  if (nrow(x) > pageLength & knitr::is_html_output()) {
    dt <- DT::datatable(x,
                        rownames = rownames,
                        caption = caption,
                        escape = escape,
                        width = "100%",
                        height = "auto",
                        options = list(pageLength = pageLength,
                                       print.keys = TRUE),
                        selection = "none")
    double_cols <- sapply(x, is.double) |> which() |> names()
    if (length(double_cols) > 0) {
      dt <- DT::formatRound(dt,
                            columns = double_cols,
                            digits = digits,
                            mark = ".",
                            dec.mark = ",")
    }
    integer_cols <- sapply(x, is.integer) |> which() |> names()
    if (length(integer_cols) > 0) {
      dt <- DT::formatRound(dt,
                            columns = integer_cols,
                            digits = 0,
                            mark = ".")
    }
    knitr::knit_print(dt, options)
  } else {
    # general formatting
    x <- x |> mutate(across(where(is.integer),
                            ~format(.x, nsmall = 0, big.mark = ".")),
                     across(where(is.double), ~format(.x, nsmall = digits,
                                                          big.mark = ".",
                                                          decimal.mark = ",")))

    # use kableExtra::kable for PDFs or shorter tables
    k <- kableExtra::kable(x,
                           digits = digits,
                           row.names = rownames,
                           caption = caption,
                           escape = escape) |>
      kableExtra::kable_styling(
        full_width = options$full_width,
        bootstrap_options = c("striped", "hover")
      )

    if (knitr::is_html_output()) {
      k <- c("<div class=\"kable-table\">", k, "</div>") |>
        paste(collapse = "\n")
    }

    knitr::asis_output(k)
  }
}
registerS3method("knit_print", "data.frame", knit_print.data.frame)
