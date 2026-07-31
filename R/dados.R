#' Ler dados de matricula por faixa etaria
#'
#' @param fonte Fonte dos dados. Use `"pacote"` para ler o snapshot Parquet
#'   instalado com o pacote ou `"motherduck"` para consultar o MotherDuck.
#' @param con Conexao opcional com MotherDuck. Usada apenas quando
#'   `fonte = "motherduck"`. Se omitida, uma nova conexao sera aberta e fechada
#'   automaticamente. Se informada sem `fonte`, a fonte MotherDuck sera usada.
#'
#' @return Data frame com matriculas por UF, ano e faixa etaria. As colunas
#'   `ETAPA_ENSINO` e `ETAPA_ENSINO_NOME` sao preenchidas com `NA`, pois os
#'   dados nao possuem recorte por etapa de ensino.
#' @export
ler_matricula_faixaetaria <- function(fonte = c("pacote", "motherduck"), con = NULL) {
  if (!missing(fonte) && missing(con) && inherits(fonte, "DBIConnection")) {
    con <- fonte
    fonte <- "motherduck"
  }

  if (!missing(con) && missing(fonte)) {
    fonte <- "motherduck"
  }

  fonte <- match.arg(fonte)

  if (identical(fonte, "pacote")) {
    return(read_package_parquet("matricula_faixaetaria.parquet"))
  }

  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(sql_path("matricula_faixaetaria.sql"), con = con)
}

#' Ler dados de matricula por faixa etaria e etapa de ensino
#'
#' @param fonte Fonte dos dados. Use `"pacote"` para ler o snapshot Parquet
#'   instalado com o pacote ou `"motherduck"` para consultar o MotherDuck.
#' @param con Conexao opcional com MotherDuck. Usada apenas quando
#'   `fonte = "motherduck"`. Se omitida, uma nova conexao sera aberta e fechada
#'   automaticamente. Se informada sem `fonte`, a fonte MotherDuck sera usada.
#'
#' @return Data frame com matriculas por UF, etapa de ensino, ano e faixa etaria.
#' @export
ler_matricula_faixaetaria_etapa <- function(fonte = c("pacote", "motherduck"), con = NULL) {
  if (!missing(fonte) && missing(con) && inherits(fonte, "DBIConnection")) {
    con <- fonte
    fonte <- "motherduck"
  }

  if (!missing(con) && missing(fonte)) {
    fonte <- "motherduck"
  }

  fonte <- match.arg(fonte)

  if (identical(fonte, "pacote")) {
    return(read_package_parquet("matricula_faixaetaria_etapa.parquet"))
  }

  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(sql_path("matricula_faixaetaria_etapa.sql"), con = con)
}

#' Ler dados de projecao populacional do IBGE
#'
#' @param fonte Fonte dos dados. Use `"pacote"` para ler o snapshot Parquet
#'   instalado com o pacote ou `"motherduck"` para consultar o MotherDuck.
#' @param con Conexao opcional com MotherDuck. Usada apenas quando
#'   `fonte = "motherduck"`. Se omitida, uma nova conexao sera aberta e fechada
#'   automaticamente. Se informada sem `fonte`, a fonte MotherDuck sera usada.
#'
#' @return Data frame com populacao projetada por UF, faixa escolar esperada e ano.
#' @export
ler_projecao_populacional_ibge <- function(fonte = c("pacote", "motherduck"), con = NULL) {
  if (!missing(fonte) && missing(con) && inherits(fonte, "DBIConnection")) {
    con <- fonte
    fonte <- "motherduck"
  }

  if (!missing(con) && missing(fonte)) {
    fonte <- "motherduck"
  }

  fonte <- match.arg(fonte)

  if (identical(fonte, "pacote")) {
    return(read_package_parquet("projecao_populacional_ibge.parquet"))
  }

  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(sql_path("projecao_populacional.sql"), con = con)
}

read_package_parquet <- function(file) {
  path <- package_data_path(file)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_parquet(%s)",
      DBI::dbQuoteString(con, path)
    )
  )
}

package_data_path <- function(file) {
  installed_path <- system.file(
    "extdata",
    file,
    package = "ofertaDemandaDocente"
  )

  if (nzchar(installed_path)) {
    return(installed_path)
  }

  development_path <- file.path("inst", "extdata", file)

  if (file.exists(development_path)) {
    return(development_path)
  }

  stop(
    sprintf(
      paste(
        "Arquivo de dados `%s` nao encontrado.",
        "Gere o snapshot Parquet em `inst/extdata/` antes de instalar o pacote",
        "ou use `fonte = \"motherduck\"`."
      ),
      file
    ),
    call. = FALSE
  )
}
