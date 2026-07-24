#' Calcular indicadores observados de matricula
#'
#' @param matriculas Data frame de matriculas por faixa etaria, como retornado
#'   por [ler_matricula_faixaetaria()].
#' @param populacao Data frame de populacao por etapa, como retornado por
#'   [ler_projecao_populacional_ibge()].
#' @param anos Vetor de anos observados a considerar. Por padrao, `2016:2025`.
#'
#' @return Data frame com uma linha por UF, etapa de ensino e ano, contendo os
#'   componentes dos calculos e os indicadores em percentual.
#' @importFrom stats aggregate
#' @export
calcular_indicadores_matricula_observados <- function(matriculas, populacao, anos = 2016:2025) {
  validar_colunas(
    matriculas,
    c("ANO", "NO_UF", "ETAPA_ENSINO", "ETAPA_ENSINO_NOME", "FAIXA_ETARIA", "QT_MAT"),
    "matriculas"
  )
  validar_colunas(
    populacao,
    c("SIGLA", "LOCAL", "ETAPA_ENSINO", "ANO", "POPULACAO"),
    "populacao"
  )

  matriculas <- matriculas[matriculas$ANO %in% anos, ]
  populacao <- populacao[populacao$ANO %in% anos, ]

  matriculas$FAIXA_ADEQUADA <- faixa_etaria_adequada(
    etapa_ensino = matriculas$ETAPA_ENSINO,
    faixa_etaria = matriculas$FAIXA_ETARIA
  )

  chaves_matricula <- c("NO_UF", "ETAPA_ENSINO", "ETAPA_ENSINO_NOME", "ANO")

  total_matriculas <- aggregate(
    QT_MAT ~ NO_UF + ETAPA_ENSINO + ETAPA_ENSINO_NOME + ANO,
    data = matriculas[matriculas$FAIXA_ETARIA == "Total", ],
    FUN = sum,
    na.rm = TRUE
  )
  names(total_matriculas)[names(total_matriculas) == "QT_MAT"] <- "TOTAL_MATRICULAS"

  matriculas_faixa_adequada <- aggregate(
    QT_MAT ~ NO_UF + ETAPA_ENSINO + ETAPA_ENSINO_NOME + ANO,
    data = matriculas[matriculas$FAIXA_ADEQUADA, ],
    FUN = sum,
    na.rm = TRUE
  )
  names(matriculas_faixa_adequada)[names(matriculas_faixa_adequada) == "QT_MAT"] <- "MAT_FAIXA_ADEQUADA"

  indicadores <- merge(
    total_matriculas,
    matriculas_faixa_adequada,
    by = chaves_matricula,
    all.x = TRUE
  )

  populacao_faixa_adequada <- populacao[, c("SIGLA", "LOCAL", "ETAPA_ENSINO", "ANO", "POPULACAO")]
  names(populacao_faixa_adequada) <- c("SIGLA_UF", "NO_UF", "ETAPA_ENSINO", "ANO", "POP_FAIXA_ADEQUADA")

  indicadores <- merge(
    indicadores,
    populacao_faixa_adequada,
    by = c("NO_UF", "ETAPA_ENSINO", "ANO"),
    all.x = TRUE
  )

  validar_indicadores_observados(indicadores)

  indicadores$MAT_FORA_FAIXA <- indicadores$TOTAL_MATRICULAS - indicadores$MAT_FAIXA_ADEQUADA
  indicadores$TAXA_LIQUIDA_MATRICULA <- taxa_liquida_matricula(
    indicadores$MAT_FAIXA_ADEQUADA,
    indicadores$POP_FAIXA_ADEQUADA
  )
  indicadores$PERCENTUAL_MATRICULAS_FORA_FAIXA <- percentual_matriculas_fora_faixa(
    indicadores$MAT_FORA_FAIXA,
    indicadores$TOTAL_MATRICULAS
  )

  indicadores <- indicadores[order(indicadores$ANO, indicadores$NO_UF, indicadores$ETAPA_ENSINO), ]
  row.names(indicadores) <- NULL
  indicadores[, c(
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
    "PERCENTUAL_MATRICULAS_FORA_FAIXA"
  )]
}

faixa_etaria_adequada <- function(etapa_ensino, faixa_etaria) {
  (etapa_ensino == "CRE" & faixa_etaria == "0 a 3 anos") |
    (etapa_ensino == "PRE" & faixa_etaria == "4 a 5 anos") |
    (etapa_ensino == "AI" & faixa_etaria == "6 a 10 anos") |
    (etapa_ensino == "AF" & faixa_etaria == "11 a 14 anos") |
    (etapa_ensino == "EM" & faixa_etaria == "15 a 17 anos")
}

validar_colunas <- function(dados, colunas, nome) {
  ausentes <- setdiff(colunas, names(dados))

  if (length(ausentes) > 0) {
    stop(
      sprintf("`%s` nao contem as colunas: %s.", nome, paste(ausentes, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validar_indicadores_observados <- function(indicadores) {
  if (anyNA(indicadores$MAT_FAIXA_ADEQUADA)) {
    stop("Ha combinacoes de UF, etapa e ano sem matriculas na faixa adequada.", call. = FALSE)
  }

  if (anyNA(indicadores$POP_FAIXA_ADEQUADA)) {
    stop("Ha combinacoes de UF, etapa e ano sem populacao correspondente.", call. = FALSE)
  }

  if (any(indicadores$MAT_FAIXA_ADEQUADA > indicadores$TOTAL_MATRICULAS)) {
    stop("Ha matriculas na faixa adequada maiores que o total de matriculas.", call. = FALSE)
  }

  invisible(TRUE)
}
