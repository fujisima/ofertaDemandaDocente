library(targets)

source("R/motherduck.R")

tar_option_set(
  packages = c("DBI", "duckdb")
)

list(
  tar_target(
    acesso_pnadc,
    motherduck_read_sql("sql/acesso_pnadc.sql")
  )
)
