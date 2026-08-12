dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("R/motherduck.R")

tratar_populacao_municipio_2025 <- function(file = "data-raw/POP2025_20260113.xls") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "O pacote `readxl` e necessario para tratar POP2025_20260113.xls.",
      call. = FALSE
    )
  }

  pop <- readxl::read_xls(
    file,
    sheet = "Municípios",
    skip = 1,
    col_types = c("text", "text", "text", "text", "numeric", "skip")
  )

  names(pop)[names(pop) == "UF"] <- "SG_UF"
  names(pop)[names(pop) == "COD. UF"] <- "CO_UF"
  names(pop)[names(pop) == "COD. MUNIC"] <- "CO_MUNICIPIO"
  names(pop)[names(pop) == "NOME DO MUNICÍPIO"] <- "NO_MUNICIPIO"
  names(pop)[names(pop) == "POPULAÇÃO ESTIMADA"] <- "POP"

  pop <- pop[!is.na(pop[["CO_UF"]]) & !is.na(pop[["CO_MUNICIPIO"]]), ]

  co_uf <- sprintf("%02d", as.integer(pop[["CO_UF"]]))
  co_municipio <- sprintf("%05d", as.integer(pop[["CO_MUNICIPIO"]]))

  pop[["CO_UF"]] <- co_uf
  pop[["CO_MUNICIPIO"]] <- paste0(co_uf, co_municipio)
  pop[["ANO"]] <- 2025L

  pop[c("SG_UF", "CO_UF", "CO_MUNICIPIO", "NO_MUNICIPIO", "ANO", "POP")]
}

write_data_frame_parquet <- function(data, parquet_file) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  path <- file.path("inst", "extdata", parquet_file)

  if (file.exists(path)) {
    unlink(path)
  }

  DBI::dbWriteTable(con, "data_to_write", data)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY data_to_write TO %s (FORMAT PARQUET)",
      DBI::dbQuoteString(con, path)
    )
  )

  invisible(path)
}

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

  write_data_frame_parquet(
    tratar_populacao_municipio_2025(),
    "pop_municipio_2025.parquet"
  )
}

gerar_dados_parquet()
