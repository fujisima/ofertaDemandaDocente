#' Calcular indicadores observados de matricula
#'
#' @param matriculas Data frame de matriculas por faixa etaria e etapa, como retornado
#'   por [ler_matricula_faixaetaria_etapa()].
#' @param populacao Data frame de populacao por etapa, como retornado por
#'   [ler_projecao_populacional_ibge()].
#' @param anos Vetor de anos observados a considerar. Por padrao, `2016:2025`.
#' @param matriculas_faixaetaria Data frame opcional de matriculas por faixa
#'   etaria, como retornado por [ler_matricula_faixaetaria()]. Quando informado,
#'   e usado para calcular a taxa bruta de matricula.
#'
#' @return Data frame com uma linha por UF, etapa de ensino e ano, contendo os
#'   componentes dos calculos e os indicadores em percentual, incluindo a taxa
#'   bruta de matricula quando `matriculas_faixaetaria` e informado.
#' @importFrom stats aggregate
#' @export
calcular_indicadores_matricula_observados <- function(
  matriculas,
  populacao,
  anos = 2016:2025,
  matriculas_faixaetaria = NULL
) {
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
  if (!is.null(matriculas_faixaetaria)) {
    validar_colunas(
      matriculas_faixaetaria,
      c("ANO", "NO_UF", "FAIXA_ETARIA", "QT_MAT"),
      "matriculas_faixaetaria"
    )
  }

  matriculas <- matriculas[matriculas$ANO %in% anos, ]
  populacao <- populacao[populacao$ANO %in% anos, ]
  if (!is.null(matriculas_faixaetaria)) {
    matriculas_faixaetaria <- matriculas_faixaetaria[matriculas_faixaetaria$ANO %in% anos, ]
  }

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

  indicadores <- adicionar_matriculas_faixa_etaria(
    indicadores = indicadores,
    matriculas_faixaetaria = matriculas_faixaetaria
  )

  indicadores$MAT_FORA_FAIXA <- indicadores$TOTAL_MATRICULAS - indicadores$MAT_FAIXA_ADEQUADA
  indicadores$TAXA_LIQUIDA_MATRICULA <- taxa_liquida_matricula(
    indicadores$MAT_FAIXA_ADEQUADA,
    indicadores$POP_FAIXA_ADEQUADA
  )
  if (all(is.na(indicadores$MAT_FAIXA_ETARIA))) {
    indicadores$TAXA_BRUTA_MATRICULA <- NA_real_
  } else {
    indicadores$TAXA_BRUTA_MATRICULA <- taxa_bruta_matricula(
      indicadores$MAT_FAIXA_ETARIA,
      indicadores$POP_FAIXA_ADEQUADA
    )
  }
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
    "MAT_FAIXA_ETARIA",
    "POP_FAIXA_ADEQUADA",
    "TAXA_LIQUIDA_MATRICULA",
    "TAXA_BRUTA_MATRICULA",
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

faixa_etaria_etapa <- function(etapa_ensino) {
  resultado <- rep(NA_character_, length(etapa_ensino))
  resultado[etapa_ensino == "CRE"] <- "0 a 3 anos"
  resultado[etapa_ensino == "PRE"] <- "4 a 5 anos"
  resultado[etapa_ensino == "AI"] <- "6 a 10 anos"
  resultado[etapa_ensino == "AF"] <- "11 a 14 anos"
  resultado[etapa_ensino == "EM"] <- "15 a 17 anos"
  resultado
}

etapa_ensino_faixa_etaria <- function(faixa_etaria) {
  resultado <- rep(NA_character_, length(faixa_etaria))
  resultado[faixa_etaria == "0 a 3 anos"] <- "CRE"
  resultado[faixa_etaria == "4 a 5 anos"] <- "PRE"
  resultado[faixa_etaria == "6 a 10 anos"] <- "AI"
  resultado[faixa_etaria == "11 a 14 anos"] <- "AF"
  resultado[faixa_etaria == "15 a 17 anos"] <- "EM"
  resultado
}

adicionar_matriculas_faixa_etaria <- function(indicadores, matriculas_faixaetaria) {
  if (is.null(matriculas_faixaetaria)) {
    indicadores$MAT_FAIXA_ETARIA <- NA_real_
    return(indicadores)
  }

  faixas_etapa <- faixa_etaria_etapa(indicadores$ETAPA_ENSINO)
  matriculas_faixaetaria <- matriculas_faixaetaria[
    matriculas_faixaetaria$FAIXA_ETARIA %in% faixas_etapa,
  ]
  matriculas_faixaetaria$ETAPA_ENSINO <- etapa_ensino_faixa_etaria(
    matriculas_faixaetaria$FAIXA_ETARIA
  )

  matriculas_faixaetaria <- aggregate(
    QT_MAT ~ NO_UF + ETAPA_ENSINO + ANO,
    data = matriculas_faixaetaria,
    FUN = sum,
    na.rm = TRUE
  )
  names(matriculas_faixaetaria)[names(matriculas_faixaetaria) == "QT_MAT"] <- "MAT_FAIXA_ETARIA"

  indicadores <- merge(
    indicadores,
    matriculas_faixaetaria,
    by = c("NO_UF", "ETAPA_ENSINO", "ANO"),
    all.x = TRUE
  )

  if (anyNA(indicadores$MAT_FAIXA_ETARIA)) {
    stop("Ha combinacoes de UF, etapa e ano sem matriculas por faixa etaria.", call. = FALSE)
  }

  indicadores
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
