#' Projetar indicadores e matriculas por UF, etapa e ano
#'
#' @param indicadores_observados Data frame de indicadores observados, como
#'   retornado por [calcular_indicadores_matricula_observados()].
#' @param populacao Data frame de populacao por etapa, como retornado por
#'   [ler_projecao_populacional_ibge()].
#' @param metas Data frame com as colunas `SIGLA_UF`, `ETAPA_ENSINO`,
#'   `INDICADOR`, `ANO_TARGET` e `TARGET`. Use `SIGLA_UF = NA` para metas gerais
#'   por etapa e uma sigla de UF para metas especificas.
#' @param anos Vetor de anos a projetar. Por padrao, `2026:2036`.
#' @param ano_base Ano observado usado como ponto inicial. Por padrao, `2025`.
#' @param lim Limite superior dos indicadores. Por padrao, `100`.
#'
#' @return Data frame com indicadores e matriculas projetadas.
#' @export
projetar_indicadores_matricula <- function(
  indicadores_observados,
  populacao,
  metas,
  anos = 2026:2036,
  ano_base = 2025,
  lim = 100
) {
  indicadores_observados <- normalizar_indicadores_observados_projecao(indicadores_observados)

  validar_colunas(
    indicadores_observados,
    c(
      "ANO",
      "SIGLA_UF",
      "NO_UF",
      "ETAPA_ENSINO",
      "ETAPA_ENSINO_NOME",
      "TAXA_LIQUIDA_MATRICULA",
      "PERCENTUAL_MATRICULAS_FORA_FAIXA"
    ),
    "indicadores_observados"
  )
  validar_colunas(
    populacao,
    c("SIGLA", "LOCAL", "ETAPA_ENSINO", "ANO", "POPULACAO"),
    "populacao"
  )
  validar_colunas(
    metas,
    c("SIGLA_UF", "ETAPA_ENSINO", "INDICADOR", "ANO_TARGET", "TARGET"),
    "metas"
  )

  indicadores_base <- indicadores_observados[indicadores_observados$ANO == ano_base, ]
  indicadores_base <- indicadores_base[
    !is.na(indicadores_base$TAXA_LIQUIDA_MATRICULA) &
      !is.na(indicadores_base$PERCENTUAL_MATRICULAS_FORA_FAIXA),
  ]

  if (nrow(indicadores_base) == 0) {
    stop(sprintf("Nao ha indicadores observados para `ano_base = %s`.", ano_base), call. = FALSE)
  }

  metas_resolvidas <- resolver_metas_indicadores(indicadores_base, metas)
  populacao_projetada <- populacao[populacao$ANO %in% anos, c("SIGLA", "LOCAL", "ETAPA_ENSINO", "ANO", "POPULACAO")]
  names(populacao_projetada) <- c("SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "ANO", "POP_FAIXA_ADEQUADA")

  resultado <- merge(
    expandir_base_projecao(indicadores_base, anos),
    populacao_projetada,
    by = c("SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "ANO"),
    all.x = TRUE
  )

  if (anyNA(resultado$POP_FAIXA_ADEQUADA)) {
    stop("Ha combinacoes de UF, etapa e ano sem populacao projetada correspondente.", call. = FALSE)
  }

  resultado <- merge(
    resultado,
    metas_resolvidas,
    by = c("SIGLA_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )

  resultado$TAXA_LIQUIDA_MATRICULA <- mapply(
    projetar_indicador,
    obs0 = resultado$TAXA_LIQUIDA_BASE,
    target = resultado$TARGET_TAXA_LIQUIDA_MATRICULA,
    tf = resultado$ANO_TARGET_TAXA_LIQUIDA_MATRICULA,
    t = resultado$ANO,
    MoreArgs = list(t0 = ano_base, lim = lim)
  )
  resultado$PERCENTUAL_MATRICULAS_FORA_FAIXA <- mapply(
    projetar_indicador,
    obs0 = resultado$PERCENTUAL_FORA_FAIXA_BASE,
    target = resultado$TARGET_PERCENTUAL_MATRICULAS_FORA_FAIXA,
    tf = resultado$ANO_TARGET_PERCENTUAL_MATRICULAS_FORA_FAIXA,
    t = resultado$ANO,
    MoreArgs = list(t0 = ano_base, lim = lim)
  )

  resultado$MAT_FAIXA_ADEQUADA <- resultado$TAXA_LIQUIDA_MATRICULA / 100 * resultado$POP_FAIXA_ADEQUADA
  resultado$TOTAL_MATRICULAS <- resultado$MAT_FAIXA_ADEQUADA /
    (1 - resultado$PERCENTUAL_MATRICULAS_FORA_FAIXA / 100)
  resultado$MAT_FORA_FAIXA <- resultado$TOTAL_MATRICULAS - resultado$MAT_FAIXA_ADEQUADA
  resultado$TIPO_DADO <- "projetado"

  resultado <- resultado[order(resultado$ANO, resultado$NO_UF, resultado$ETAPA_ENSINO), ]
  row.names(resultado) <- NULL
  resultado[, c(
    "ANO",
    "SIGLA_UF",
    "NO_UF",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "TOTAL_MATRICULAS",
    "MAT_FAIXA_ADEQUADA",
    "MAT_FORA_FAIXA",
    "POP_FAIXA_ADEQUADA",
    "TAXA_LIQUIDA_MATRICULA",
    "PERCENTUAL_MATRICULAS_FORA_FAIXA",
    "TIPO_DADO"
  )]
}

normalizar_indicadores_observados_projecao <- function(indicadores_observados) {
  if (!"ETAPA_ENSINO_ADEQUADA" %in% names(indicadores_observados)) {
    return(indicadores_observados)
  }

  validar_colunas(
    indicadores_observados,
    c("ETAPA_ENSINO_ADEQUADA", "ETAPA_ENSINO_ADEQUADA_NOME"),
    "indicadores_observados"
  )

  indicadores_observados$ETAPA_ENSINO <- indicadores_observados$ETAPA_ENSINO_ADEQUADA
  indicadores_observados$ETAPA_ENSINO_NOME <- indicadores_observados$ETAPA_ENSINO_ADEQUADA_NOME
  indicadores_observados
}

resolver_metas_indicadores <- function(indicadores_base, metas) {
  combinacoes <- unique(indicadores_base[, c("SIGLA_UF", "ETAPA_ENSINO")])

  metas_taxa <- resolver_metas_indicador(
    combinacoes = combinacoes,
    metas = metas,
    indicador = "TAXA_LIQUIDA_MATRICULA"
  )
  metas_fora_faixa <- resolver_metas_indicador(
    combinacoes = combinacoes,
    metas = metas,
    indicador = "PERCENTUAL_MATRICULAS_FORA_FAIXA"
  )

  merge(metas_taxa, metas_fora_faixa, by = c("SIGLA_UF", "ETAPA_ENSINO"))
}

resolver_metas_indicador <- function(combinacoes, metas, indicador) {
  metas_indicador <- metas[metas$INDICADOR == indicador, ]
  metas_especificas <- metas_indicador[!is.na(metas_indicador$SIGLA_UF), ]
  metas_gerais <- metas_indicador[is.na(metas_indicador$SIGLA_UF), c("ETAPA_ENSINO", "ANO_TARGET", "TARGET")]

  resultado <- merge(
    combinacoes,
    metas_especificas[, c("SIGLA_UF", "ETAPA_ENSINO", "ANO_TARGET", "TARGET")],
    by = c("SIGLA_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )
  resultado <- merge(
    resultado,
    metas_gerais,
    by = "ETAPA_ENSINO",
    all.x = TRUE,
    suffixes = c("_ESPECIFICO", "_GERAL")
  )

  resultado$ANO_TARGET <- ifelse(
    is.na(resultado$ANO_TARGET_ESPECIFICO),
    resultado$ANO_TARGET_GERAL,
    resultado$ANO_TARGET_ESPECIFICO
  )
  resultado$TARGET <- ifelse(
    is.na(resultado$TARGET_ESPECIFICO),
    resultado$TARGET_GERAL,
    resultado$TARGET_ESPECIFICO
  )

  if (anyNA(resultado$TARGET) || anyNA(resultado$ANO_TARGET)) {
    faltantes <- resultado[is.na(resultado$TARGET) | is.na(resultado$ANO_TARGET), c("SIGLA_UF", "ETAPA_ENSINO")]
    stop(
      sprintf(
        "Ha combinacoes sem meta para `%s`: %s.",
        indicador,
        paste(paste(faltantes$SIGLA_UF, faltantes$ETAPA_ENSINO, sep = "/"), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  names(resultado)[names(resultado) == "ANO_TARGET"] <- paste0("ANO_TARGET_", indicador)
  names(resultado)[names(resultado) == "TARGET"] <- paste0("TARGET_", indicador)
  resultado[, c("SIGLA_UF", "ETAPA_ENSINO", paste0("ANO_TARGET_", indicador), paste0("TARGET_", indicador))]
}

expandir_base_projecao <- function(indicadores_base, anos) {
  base <- indicadores_base[, c(
    "SIGLA_UF",
    "NO_UF",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "TAXA_LIQUIDA_MATRICULA",
    "PERCENTUAL_MATRICULAS_FORA_FAIXA"
  )]
  names(base)[names(base) == "TAXA_LIQUIDA_MATRICULA"] <- "TAXA_LIQUIDA_BASE"
  names(base)[names(base) == "PERCENTUAL_MATRICULAS_FORA_FAIXA"] <- "PERCENTUAL_FORA_FAIXA_BASE"

  expandido <- base[rep(seq_len(nrow(base)), each = length(anos)), ]
  expandido$ANO <- rep(anos, times = nrow(base))
  expandido
}

projetar_indicador <- function(obs0, target, t0, tf, t, lim) {
  projecao_logistica_target(
    t0 = t0,
    tf = tf,
    lim = lim,
    obs0 = obs0,
    target = target,
    t = t
  )
}
