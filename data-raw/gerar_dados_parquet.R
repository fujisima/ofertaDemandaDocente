dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("R/motherduck.R")

gerar_dados_parquet <- function() {
  con <- motherduck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  write_query_parquet <- function(sql_file, parquet_file) {
    sql <- paste(readLines(sql_file, warn = FALSE), collapse = "\n")
    sql <- sub(";\\s*$", "", sql)
    path <- file.path("inst", "extdata", parquet_file)

    if (file.exists(path)) {
      unlink(path)
    }

    DBI::dbExecute(
      con,
      sprintf(
        "COPY (%s) TO %s (FORMAT PARQUET)",
        sql,
        DBI::dbQuoteString(con, path)
      )
    )

    invisible(path)
  }

  write_query_parquet(
    "sql/matricula_faixaetaria.sql",
    "matricula_faixaetaria.parquet"
  )

  write_query_parquet(
    "sql/matricula_faixaetaria_etapa.sql",
    "matricula_faixaetaria_etapa.parquet"
  )

  write_query_parquet(
    "sql/projecao_populacional.sql",
    "projecao_populacional_ibge.parquet"
  )
}

gerar_dados_parquet()
