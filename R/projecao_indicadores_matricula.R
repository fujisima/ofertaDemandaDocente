#' Projetar indicadores e matriculas por UF, faixa etaria e ano
#'
#' @param indicadores_observados Data frame de indicadores observados, como
#'   retornado por [calcular_indicadores_matricula_observados()].
#' @param populacao Data frame de populacao por etapa, como retornado por
#'   [ler_projecao_populacional_ibge()].
#' @param metas Data frame com as colunas `SIGLA_UF`, `FAIXA_ETARIA`,
#'   `INDICADOR`, `ANO_TARGET` e `TARGET`. Use `SIGLA_UF = NA` para metas gerais
#'   por faixa etaria e uma sigla de UF para metas especificas.
#' @param anos Vetor de anos a projetar. Por padrao, `2026:2036`.
#' @param ano_base Ano observado usado como ponto inicial. Por padrao, `2025`.
#' @param lim Limite superior dos indicadores. Mantido por compatibilidade e
#'   nao usado na projecao linear.
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
      "FAIXA_ETARIA",
      "ETAPA_ENSINO",
      "ETAPA_ENSINO_NOME",
      "RZ_AVANCADOS",
      "RZ_DEF_1",
      "RZ_DEF_2",
      "TAXA_LIQUIDA_MATRICULA",
      "TAXA_BRUTA_MATRICULA"
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
    c("SIGLA_UF", "FAIXA_ETARIA", "INDICADOR", "ANO_TARGET", "TARGET"),
    "metas"
  )

  indicadores_base <- indicadores_observados[indicadores_observados$ANO == ano_base, ]
  indicadores_base <- indicadores_base[
    !is.na(indicadores_base$TAXA_BRUTA_MATRICULA),
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
    by = c("SIGLA_UF", "FAIXA_ETARIA"),
    all.x = TRUE
  )

  resultado$TAXA_BRUTA_MATRICULA <- mapply(
    projetar_indicador,
    obs0 = resultado$TAXA_BRUTA_BASE,
    target = resultado$TARGET_TAXA_BRUTA_MATRICULA,
    tf = resultado$ANO_TARGET_TAXA_BRUTA_MATRICULA,
    t = resultado$ANO,
    MoreArgs = list(t0 = ano_base, lim = lim)
  )
  resultado$TAXA_LIQUIDA_MATRICULA <- mapply(
    projetar_indicador,
    obs0 = resultado$TAXA_LIQUIDA_BASE,
    target = resultado$TARGET_TAXA_LIQUIDA_MATRICULA,
    tf = resultado$ANO_TARGET_TAXA_LIQUIDA_MATRICULA,
    t = resultado$ANO,
    MoreArgs = list(t0 = ano_base, lim = lim)
  )
  resultado$TAXA_LIQUIDA_MATRICULA[resultado$FAIXA_ETARIA == "18 a 19 anos"] <- 0

  resultado$MAT_FAIXA_ADEQUADA <- resultado$TAXA_LIQUIDA_MATRICULA / 100 * resultado$POP_FAIXA_ADEQUADA
  resultado$MAT_AVANCADOS <- matriculas_fluxo_projetadas(
    taxa_bruta = resultado$TAXA_BRUTA_MATRICULA,
    taxa_liquida = resultado$TAXA_LIQUIDA_MATRICULA,
    razao = substituir_na_por_zero(resultado$RZ_AVANCADOS_BASE),
    populacao = resultado$POP_FAIXA_ADEQUADA
  )
  resultado$MAT_DEFASAGEM_I <- matriculas_fluxo_projetadas(
    taxa_bruta = resultado$TAXA_BRUTA_MATRICULA,
    taxa_liquida = resultado$TAXA_LIQUIDA_MATRICULA,
    razao = substituir_na_por_zero(resultado$RZ_DEF_1_BASE),
    populacao = resultado$POP_FAIXA_ADEQUADA
  )
  resultado$MAT_DEFASAGEM_II <- matriculas_fluxo_projetadas(
    taxa_bruta = resultado$TAXA_BRUTA_MATRICULA,
    taxa_liquida = resultado$TAXA_LIQUIDA_MATRICULA,
    razao = substituir_na_por_zero(resultado$RZ_DEF_2_BASE),
    populacao = resultado$POP_FAIXA_ADEQUADA
  )
  resultado$TOTAL_MATRICULAS <- resultado$TAXA_BRUTA_MATRICULA / 100 * resultado$POP_FAIXA_ADEQUADA
  resultado$MAT_FORA_FAIXA <- resultado$TOTAL_MATRICULAS - resultado$MAT_FAIXA_ADEQUADA
  resultado$TIPO_DADO <- "projetado"

  resultado <- resultado[order(resultado$ANO, resultado$NO_UF, resultado$ETAPA_ENSINO), ]
  row.names(resultado) <- NULL
  resultado[, c(
    "ANO",
    "SIGLA_UF",
    "NO_UF",
    "FAIXA_ETARIA",
    "TOTAL_MATRICULAS",
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II",
    "MAT_FORA_FAIXA",
    "POP_FAIXA_ADEQUADA",
    "TAXA_BRUTA_MATRICULA",
    "TAXA_LIQUIDA_MATRICULA",
    "TIPO_DADO"
  )]
}

#' Compor matriculas projetadas por etapa de ensino
#'
#' @param indicadores_matricula_projetados Data frame de indicadores e
#'   matriculas projetadas por faixa etaria, como retornado por
#'   [projetar_indicadores_matricula()].
#'
#' @return Data frame com matriculas projetadas por UF, etapa de ensino e ano,
#'   com o total de matriculas em cada etapa.
#' @export
compor_matriculas_etapa_projetadas <- function(indicadores_matricula_projetados) {
  validar_colunas(
    indicadores_matricula_projetados,
    c(
      "ANO",
      "SIGLA_UF",
      "NO_UF",
      "FAIXA_ETARIA",
      "MAT_FAIXA_ADEQUADA",
      "MAT_AVANCADOS",
      "MAT_DEFASAGEM_I",
      "MAT_DEFASAGEM_II"
    ),
    "indicadores_matricula_projetados"
  )

  regras <- regras_matriculas_etapa_projetadas()

  adequadas <- merge(
    indicadores_matricula_projetados[, c("ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA", "MAT_FAIXA_ADEQUADA")],
    regras[regras$COMPONENTE == "MAT_FAIXA_ADEQUADA", c("ETAPA_ENSINO", "FAIXA_ETARIA", "COMPONENTE")],
    by = "FAIXA_ETARIA",
    all.x = FALSE,
    all.y = FALSE
  )
  names(adequadas)[names(adequadas) == "MAT_FAIXA_ADEQUADA"] <- "VALOR"

  avancados <- merge(
    indicadores_matricula_projetados[, c("ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA", "MAT_AVANCADOS")],
    regras[regras$COMPONENTE == "MAT_AVANCADOS", c("ETAPA_ENSINO", "FAIXA_ETARIA", "COMPONENTE")],
    by = "FAIXA_ETARIA",
    all.x = FALSE,
    all.y = FALSE
  )
  names(avancados)[names(avancados) == "MAT_AVANCADOS"] <- "VALOR"

  def_1 <- merge(
    indicadores_matricula_projetados[, c("ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA", "MAT_DEFASAGEM_I")],
    regras[regras$COMPONENTE == "MAT_DEFASAGEM_I", c("ETAPA_ENSINO", "FAIXA_ETARIA", "COMPONENTE")],
    by = "FAIXA_ETARIA",
    all.x = FALSE,
    all.y = FALSE
  )
  names(def_1)[names(def_1) == "MAT_DEFASAGEM_I"] <- "VALOR"

  def_2 <- merge(
    indicadores_matricula_projetados[, c("ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA", "MAT_DEFASAGEM_II")],
    regras[regras$COMPONENTE == "MAT_DEFASAGEM_II", c("ETAPA_ENSINO", "FAIXA_ETARIA", "COMPONENTE")],
    by = "FAIXA_ETARIA",
    all.x = FALSE,
    all.y = FALSE
  )
  names(def_2)[names(def_2) == "MAT_DEFASAGEM_II"] <- "VALOR"

  componentes <- rbind(
    adequadas[, c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "COMPONENTE", "VALOR")],
    avancados[, c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "COMPONENTE", "VALOR")],
    def_1[, c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "COMPONENTE", "VALOR")],
    def_2[, c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "COMPONENTE", "VALOR")]
  )

  componentes$VALOR[is.na(componentes$VALOR)] <- 0

  totais <- aggregate(
    VALOR ~ ANO + SIGLA_UF + NO_UF + ETAPA_ENSINO + COMPONENTE,
    data = componentes,
    FUN = sum,
    na.rm = TRUE
  )

  etapas <- data.frame(
    ETAPA_ENSINO = c("CRE", "PRE", "AI", "AF", "EM"),
    ETAPA_ENSINO_NOME = c("Creche", "Pre-escola", "Anos iniciais", "Anos finais", "Ensino medio"),
    stringsAsFactors = FALSE
  )

  combinacoes <- unique(totais[, c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO")])
  combinacoes <- merge(combinacoes, etapas, by = "ETAPA_ENSINO", all.x = TRUE)

  resultado <- merge(
    combinacoes,
    totais[totais$COMPONENTE == "MAT_FAIXA_ADEQUADA", c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "VALOR")],
    by = c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )
  names(resultado)[names(resultado) == "VALOR"] <- "MAT_FAIXA_ADEQUADA"

  resultado <- merge(
    resultado,
    totais[totais$COMPONENTE == "MAT_AVANCADOS", c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "VALOR")],
    by = c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )
  names(resultado)[names(resultado) == "VALOR"] <- "MAT_AVANCADOS"

  resultado <- merge(
    resultado,
    totais[totais$COMPONENTE == "MAT_DEFASAGEM_I", c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "VALOR")],
    by = c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )
  names(resultado)[names(resultado) == "VALOR"] <- "MAT_DEFASAGEM_I"

  resultado <- merge(
    resultado,
    totais[totais$COMPONENTE == "MAT_DEFASAGEM_II", c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "VALOR")],
    by = c("ANO", "SIGLA_UF", "NO_UF", "ETAPA_ENSINO"),
    all.x = TRUE
  )
  names(resultado)[names(resultado) == "VALOR"] <- "MAT_DEFASAGEM_II"

  componentes_numericos <- c(
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II"
  )
  for (coluna in componentes_numericos) {
    resultado[[coluna]][is.na(resultado[[coluna]])] <- 0
  }

  resultado$TOTAL_MATRICULAS <- rowSums(resultado[, componentes_numericos])
  ordem_etapa <- match(resultado$ETAPA_ENSINO, c("CRE", "PRE", "AI", "AF", "EM"))
  resultado <- resultado[order(resultado$ANO, resultado$NO_UF, ordem_etapa), ]
  row.names(resultado) <- NULL

  resultado[, c(
    "ANO",
    "SIGLA_UF",
    "NO_UF",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "TOTAL_MATRICULAS"
  )]
}

regras_matriculas_etapa_projetadas <- function() {
  data.frame(
    ETAPA_ENSINO = c(
      "CRE", "CRE", "CRE",
      "PRE", "PRE", "PRE",
      "AI", "AI", "AI", "AI",
      "AF", "AF", "AF", "AF",
      "EM", "EM", "EM"
    ),
    FAIXA_ETARIA = c(
      "0 a 3 anos", "4 a 5 anos", "6 a 10 anos",
      "4 a 5 anos", "0 a 3 anos", "6 a 10 anos",
      "6 a 10 anos", "4 a 5 anos", "11 a 14 anos", "15 a 17 anos",
      "11 a 14 anos", "6 a 10 anos", "15 a 17 anos", "18 a 19 anos",
      "15 a 17 anos", "11 a 14 anos", "18 a 19 anos"
    ),
    COMPONENTE = c(
      "MAT_FAIXA_ADEQUADA", "MAT_DEFASAGEM_I", "MAT_DEFASAGEM_II",
      "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS", "MAT_DEFASAGEM_I",
      "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS", "MAT_DEFASAGEM_I", "MAT_DEFASAGEM_II",
      "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS", "MAT_DEFASAGEM_I", "MAT_DEFASAGEM_II",
      "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS", "MAT_DEFASAGEM_I"
    ),
    stringsAsFactors = FALSE
  )
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
  combinacoes <- unique(indicadores_base[, c("SIGLA_UF", "FAIXA_ETARIA", "TAXA_LIQUIDA_MATRICULA")])

  metas_taxa_bruta <- resolver_metas_indicador(
    combinacoes = combinacoes,
    metas = metas,
    indicador = "TAXA_BRUTA_MATRICULA",
    exigir_meta = rep(TRUE, nrow(combinacoes))
  )
  metas_taxa_liquida <- resolver_metas_indicador(
    combinacoes = combinacoes,
    metas = metas,
    indicador = "TAXA_LIQUIDA_MATRICULA",
    exigir_meta = combinacoes$FAIXA_ETARIA != "18 a 19 anos" &
      !is.na(combinacoes$TAXA_LIQUIDA_MATRICULA)
  )

  merge(
    metas_taxa_bruta,
    metas_taxa_liquida,
    by = c("SIGLA_UF", "FAIXA_ETARIA"),
    all.x = TRUE
  )
}

resolver_metas_indicador <- function(combinacoes, metas, indicador, exigir_meta) {
  metas_indicador <- metas[metas$INDICADOR == indicador, ]
  metas_especificas <- metas_indicador[!is.na(metas_indicador$SIGLA_UF), ]
  metas_gerais <- metas_indicador[is.na(metas_indicador$SIGLA_UF), c("FAIXA_ETARIA", "ANO_TARGET", "TARGET")]

  resultado <- merge(
    combinacoes[, c("SIGLA_UF", "FAIXA_ETARIA")],
    metas_especificas[, c("SIGLA_UF", "FAIXA_ETARIA", "ANO_TARGET", "TARGET")],
    by = c("SIGLA_UF", "FAIXA_ETARIA"),
    all.x = TRUE
  )
  resultado <- merge(
    resultado,
    metas_gerais,
    by = "FAIXA_ETARIA",
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

  chaves_resultado <- paste(resultado$SIGLA_UF, resultado$FAIXA_ETARIA, sep = "\r")
  chaves_combinacoes <- paste(combinacoes$SIGLA_UF, combinacoes$FAIXA_ETARIA, sep = "\r")
  resultado$EXIGIR_META <- exigir_meta[match(chaves_resultado, chaves_combinacoes)]

  if (any((is.na(resultado$TARGET) | is.na(resultado$ANO_TARGET)) & resultado$EXIGIR_META)) {
    faltantes <- resultado[
      (is.na(resultado$TARGET) | is.na(resultado$ANO_TARGET)) & resultado$EXIGIR_META,
      c("SIGLA_UF", "FAIXA_ETARIA")
    ]
    stop(
      sprintf(
        "Ha combinacoes sem meta para `%s`: %s.",
        indicador,
        paste(paste(faltantes$SIGLA_UF, faltantes$FAIXA_ETARIA, sep = "/"), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  names(resultado)[names(resultado) == "ANO_TARGET"] <- paste0("ANO_TARGET_", indicador)
  names(resultado)[names(resultado) == "TARGET"] <- paste0("TARGET_", indicador)
  resultado[, c("SIGLA_UF", "FAIXA_ETARIA", paste0("ANO_TARGET_", indicador), paste0("TARGET_", indicador))]
}

expandir_base_projecao <- function(indicadores_base, anos) {
  base <- indicadores_base[, c(
    "SIGLA_UF",
    "NO_UF",
    "FAIXA_ETARIA",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "RZ_AVANCADOS",
    "RZ_DEF_1",
    "RZ_DEF_2",
    "TAXA_LIQUIDA_MATRICULA",
    "TAXA_BRUTA_MATRICULA"
  )]
  names(base)[names(base) == "TAXA_LIQUIDA_MATRICULA"] <- "TAXA_LIQUIDA_BASE"
  names(base)[names(base) == "TAXA_BRUTA_MATRICULA"] <- "TAXA_BRUTA_BASE"
  names(base)[names(base) == "RZ_AVANCADOS"] <- "RZ_AVANCADOS_BASE"
  names(base)[names(base) == "RZ_DEF_1"] <- "RZ_DEF_1_BASE"
  names(base)[names(base) == "RZ_DEF_2"] <- "RZ_DEF_2_BASE"

  expandido <- base[rep(seq_len(nrow(base)), each = length(anos)), ]
  expandido$ANO <- rep(anos, times = nrow(base))
  expandido
}

projetar_indicador <- function(obs0, target, t0, tf, t, lim) {
  if (is.na(obs0) || is.na(target) || is.na(tf)) {
    return(NA_real_)
  }

  obs0 + ((target - obs0) / (tf - t0)) * (t - t0)
}

matriculas_fluxo_projetadas <- function(taxa_bruta, taxa_liquida, razao, populacao) {
  resultado <- rep(NA_real_, length(taxa_bruta))
  presentes <- !is.na(taxa_bruta) &
    !is.na(taxa_liquida) &
    !is.na(razao) &
    !is.na(populacao)

  if (any(presentes)) {
    resultado[presentes] <- (taxa_bruta[presentes] - taxa_liquida[presentes]) *
      razao[presentes] / 100 * populacao[presentes]
  }

  resultado
}

substituir_na_por_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}
