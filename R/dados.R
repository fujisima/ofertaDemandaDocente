#' Ler dados de matricula por faixa etaria
#'
#' @param con Conexao opcional com MotherDuck. Se omitida, uma nova conexao sera
#'   aberta e fechada automaticamente.
#'
#' @return Data frame com matriculas por UF, etapa de ensino, ano e faixa etaria.
#' @export
ler_matricula_faixaetaria <- function(con = NULL) {
  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(sql_path("matricula_faixaetaria.sql"), con = con)
}

#' Ler dados de projecao populacional do IBGE
#'
#' @param con Conexao opcional com MotherDuck. Se omitida, uma nova conexao sera
#'   aberta e fechada automaticamente.
#'
#' @return Data frame com populacao projetada por UF, etapa de ensino e ano.
#' @export
ler_projecao_populacional_ibge <- function(con = NULL) {
  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(sql_path("projecao_populacional.sql"), con = con)
}
