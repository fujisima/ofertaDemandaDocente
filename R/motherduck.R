motherduck_connect <- function(dbdir = ":memory:", database = "fujisima_db") {
  token <- Sys.getenv("MOTHERDUCK_TOKEN")

  if (!nzchar(token)) {
    token <- Sys.getenv("motherduck_token")
  }

  if (!nzchar(token)) {
    stop(
      "MOTHERDUCK_TOKEN nao encontrado. Configure o token no arquivo .Renviron.",
      call. = FALSE
    )
  }

  Sys.setenv(MOTHERDUCK_TOKEN = token)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
  tryCatch(
    DBI::dbExecute(con, "LOAD motherduck"),
    error = function(err) {
      DBI::dbExecute(con, "INSTALL motherduck")
      DBI::dbExecute(con, "LOAD motherduck")
    }
  )
  DBI::dbExecute(
    con,
    sprintf(
      "ATTACH IF NOT EXISTS 'md:%s' AS %s",
      database,
      database
    )
  )
  con
}

motherduck_read_sql <- function(path, con = motherduck_connect()) {
  sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
  DBI::dbGetQuery(con, sql)
}

sql_path <- function(file) {
  configured_dir <- getOption("ofertaDemandaDocente.sql_dir")

  if (!is.null(configured_dir)) {
    configured_path <- file.path(configured_dir, file)

    if (file.exists(configured_path)) {
      return(configured_path)
    }
  }

  installed_path <- system.file("sql", file, package = "ofertaDemandaDocente")

  if (nzchar(installed_path)) {
    return(installed_path)
  }

  development_path <- file.path("sql", file)

  if (file.exists(development_path)) {
    return(development_path)
  }

  stop(sprintf("Arquivo SQL `%s` nao encontrado.", file), call. = FALSE)
}
