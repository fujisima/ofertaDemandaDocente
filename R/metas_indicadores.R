#' Criar tabela de metas gerais para indicadores de matricula
#'
#' @param taxa_bruta_matricula Vetor numerico nomeado com metas da taxa bruta de
#'   matricula por faixa etaria.
#' @param taxa_liquida_matricula Vetor numerico nomeado com metas da taxa
#'   liquida de matricula por faixa etaria. Nao deve incluir a faixa
#'   `"18 a 19 anos"`, para a qual nao ha taxa liquida.
#' @param ano_target Ano em que as metas devem ser atingidas.
#'
#' @return Data frame no formato esperado por [projetar_indicadores_matricula()].
#' @export
criar_metas_indicadores_gerais <- function(
  taxa_bruta_matricula,
  taxa_liquida_matricula,
  ano_target = 2036
) {
  if ("18 a 19 anos" %in% names(taxa_liquida_matricula)) {
    stop("A faixa `18 a 19 anos` nao deve ter meta de `taxa_liquida_matricula`.", call. = FALSE)
  }

  metas_taxa_bruta <- criar_metas_indicador(
    valores = taxa_bruta_matricula,
    indicador = "TAXA_BRUTA_MATRICULA",
    ano_target = ano_target
  )
  metas_taxa_liquida <- criar_metas_indicador(
    valores = taxa_liquida_matricula,
    indicador = "TAXA_LIQUIDA_MATRICULA",
    ano_target = ano_target
  )

  rbind(metas_taxa_bruta, metas_taxa_liquida)
}

criar_metas_indicador <- function(valores, indicador, ano_target) {
  if (is.null(names(valores)) || any(!nzchar(names(valores)))) {
    stop("As metas devem ser vetores nomeados por `FAIXA_ETARIA`.", call. = FALSE)
  }

  data.frame(
    SIGLA_UF = NA_character_,
    FAIXA_ETARIA = names(valores),
    INDICADOR = indicador,
    ANO_TARGET = ano_target,
    TARGET = as.numeric(valores),
    stringsAsFactors = FALSE
  )
}
