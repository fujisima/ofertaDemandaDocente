#' Criar tabela de metas gerais para indicadores de matricula
#'
#' @param taxa_liquida_matricula Vetor numerico nomeado com metas da taxa
#'   liquida de matricula por etapa de ensino.
#' @param percentual_matriculas_fora_faixa Vetor numerico nomeado com metas do
#'   percentual de matriculas fora da faixa etaria por etapa de ensino.
#' @param ano_target Ano em que as metas devem ser atingidas.
#'
#' @return Data frame no formato esperado por [projetar_indicadores_matricula()].
#' @export
criar_metas_indicadores_gerais <- function(
  taxa_liquida_matricula,
  percentual_matriculas_fora_faixa,
  ano_target
) {
  metas_taxa <- criar_metas_indicador(
    valores = taxa_liquida_matricula,
    indicador = "TAXA_LIQUIDA_MATRICULA",
    ano_target = ano_target
  )
  metas_fora_faixa <- criar_metas_indicador(
    valores = percentual_matriculas_fora_faixa,
    indicador = "PERCENTUAL_MATRICULAS_FORA_FAIXA",
    ano_target = ano_target
  )

  rbind(metas_taxa, metas_fora_faixa)
}

criar_metas_indicador <- function(valores, indicador, ano_target) {
  if (is.null(names(valores)) || any(!nzchar(names(valores)))) {
    stop("As metas devem ser vetores nomeados por `ETAPA_ENSINO`.", call. = FALSE)
  }

  data.frame(
    SIGLA_UF = NA_character_,
    ETAPA_ENSINO = names(valores),
    INDICADOR = indicador,
    ANO_TARGET = ano_target,
    TARGET = as.numeric(valores),
    stringsAsFactors = FALSE
  )
}
