dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

source("R/motherduck.R")

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
  "sql/projecao_populacional.sql",
  "projecao_populacional_ibge.parquet"
)
