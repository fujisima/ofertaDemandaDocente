#' Calcular indicadores observados de matricula
#'
#' @param matriculas Data frame de matriculas por faixa etaria e etapa, como retornado
#'   por [ler_matricula_faixaetaria_etapa()].
#' @param populacao Data frame de populacao por etapa, como retornado por
#'   [ler_projecao_populacional_ibge()].
#' @param anos Vetor de anos observados a considerar. Por padrao, `2016:2025`.
#' @param matriculas_faixaetaria Data frame opcional de matriculas por faixa
#'   etaria, como retornado por [ler_matricula_faixaetaria()]. Quando informado,
#'   e incluido como total amplo por faixa etaria.
#'
#' @return Data frame com uma linha por UF, faixa etaria e ano, contendo os
#'   componentes dos calculos e os indicadores em percentual. A coluna
#'   `ETAPA_ENSINO_ADEQUADA` identifica a etapa esperada para a faixa etaria,
#'   quando aplicavel. A taxa bruta de matricula usa como numerador a soma das
#'   matriculas na faixa adequada, avancados, defasagem I e defasagem II;
#'   `MAT_FAIXA_ETARIA` mantem o total amplo por faixa etaria quando
#'   `matriculas_faixaetaria` e informado. As taxas de avancados e defasagem
#'   usam a populacao da faixa etaria como denominador; `TIPO_CALCULO_*` indica
#'   se o numerador e exato, aproximado ou nao aplicavel.
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

  mapeamento_faixas <- mapeamento_faixas_etarias_indicadores()
  chaves_etapa <- c("NO_UF", "ETAPA_ENSINO_ADEQUADA", "ANO")

  total_matriculas <- aggregate(
    QT_MAT ~ NO_UF + ETAPA_ENSINO + ANO,
    data = matriculas[matriculas$FAIXA_ETARIA == "Total", ],
    FUN = sum,
    na.rm = TRUE
  )
  names(total_matriculas)[names(total_matriculas) == "ETAPA_ENSINO"] <- "ETAPA_ENSINO_ADEQUADA"
  names(total_matriculas)[names(total_matriculas) == "QT_MAT"] <- "TOTAL_MATRICULAS"

  matriculas_faixa_adequada <- aggregate(
    QT_MAT ~ NO_UF + ETAPA_ENSINO + ANO,
    data = matriculas[matriculas$FAIXA_ADEQUADA, ],
    FUN = sum,
    na.rm = TRUE
  )
  names(matriculas_faixa_adequada)[names(matriculas_faixa_adequada) == "ETAPA_ENSINO"] <- "ETAPA_ENSINO_ADEQUADA"
  names(matriculas_faixa_adequada)[names(matriculas_faixa_adequada) == "QT_MAT"] <- "MAT_FAIXA_ADEQUADA"

  populacao_faixa_adequada <- populacao[, c("SIGLA", "LOCAL", "ETAPA_ENSINO", "ANO", "POPULACAO")]
  names(populacao_faixa_adequada) <- c(
    "SIGLA_UF",
    "NO_UF",
    "ETAPA_ENSINO_ADEQUADA",
    "ANO",
    "POP_FAIXA_ADEQUADA"
  )
  populacao_faixa_adequada <- merge(
    populacao_faixa_adequada,
    mapeamento_faixas,
    by = "ETAPA_ENSINO_ADEQUADA",
    all.x = FALSE
  )

  chaves_total_matriculas <- paste(
    total_matriculas$NO_UF,
    total_matriculas$ETAPA_ENSINO_ADEQUADA,
    total_matriculas$ANO,
    sep = "\r"
  )
  chaves_populacao <- paste(
    populacao_faixa_adequada$NO_UF,
    populacao_faixa_adequada$ETAPA_ENSINO_ADEQUADA,
    populacao_faixa_adequada$ANO,
    sep = "\r"
  )

  if (any(!chaves_total_matriculas %in% chaves_populacao)) {
    stop("Ha combinacoes de UF, etapa e ano sem populacao correspondente.", call. = FALSE)
  }

  indicadores <- merge(
    populacao_faixa_adequada,
    total_matriculas,
    by = chaves_etapa,
    all.x = TRUE
  )
  indicadores <- merge(
    indicadores,
    matriculas_faixa_adequada,
    by = chaves_etapa,
    all.x = TRUE
  )

  validar_indicadores_observados(indicadores)

  indicadores <- adicionar_matriculas_faixa_etaria(
    indicadores = indicadores,
    matriculas_faixaetaria = matriculas_faixaetaria
  )
  indicadores <- adicionar_indicadores_fluxo_etario(
    indicadores = indicadores,
    matriculas = matriculas
  )

  indicadores$MAT_FORA_FAIXA <- indicadores$TOTAL_MATRICULAS - indicadores$MAT_FAIXA_ADEQUADA
  indicadores$TAXA_LIQUIDA_MATRICULA <- taxa_matricula_opcional(
    indicadores$MAT_FAIXA_ADEQUADA,
    indicadores$POP_FAIXA_ADEQUADA
  )
  indicadores$MAT_TAXA_BRUTA_MATRICULA <- numerador_taxa_bruta_matricula(indicadores)
  indicadores$TAXA_BRUTA_MATRICULA <- taxa_bruta_matricula(
    indicadores$MAT_TAXA_BRUTA_MATRICULA,
    indicadores$POP_FAIXA_ADEQUADA
  )
  indicadores$PERCENTUAL_MATRICULAS_FORA_FAIXA <- percentual_matriculas_fora_faixa_opcional(
    indicadores$MAT_FORA_FAIXA,
    indicadores$TOTAL_MATRICULAS
  )
  indicadores$TAXA_AVANCADOS <- taxa_matricula_opcional(
    indicadores$MAT_AVANCADOS,
    indicadores$POP_FAIXA_ADEQUADA
  )
  indicadores$TAXA_DEFASAGEM_I <- taxa_matricula_opcional(
    indicadores$MAT_DEFASAGEM_I,
    indicadores$POP_FAIXA_ADEQUADA
  )
  indicadores$TAXA_DEFASAGEM_II <- taxa_matricula_opcional(
    indicadores$MAT_DEFASAGEM_II,
    indicadores$POP_FAIXA_ADEQUADA
  )

  indicadores <- indicadores[order(indicadores$ANO, indicadores$NO_UF, indicadores$ORDEM_FAIXA_ETARIA), ]
  row.names(indicadores) <- NULL
  indicadores[, c(
    "ANO",
    "SIGLA_UF",
    "NO_UF",
    "FAIXA_ETARIA",
    "ETAPA_ENSINO_ADEQUADA",
    "ETAPA_ENSINO_ADEQUADA_NOME",
    "TOTAL_MATRICULAS",
    "MAT_FAIXA_ADEQUADA",
    "MAT_FORA_FAIXA",
    "MAT_FAIXA_ETARIA",
    "MAT_TAXA_BRUTA_MATRICULA",
    "POP_FAIXA_ADEQUADA",
    "TAXA_LIQUIDA_MATRICULA",
    "TAXA_BRUTA_MATRICULA",
    "PERCENTUAL_MATRICULAS_FORA_FAIXA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II",
    "TAXA_AVANCADOS",
    "TAXA_DEFASAGEM_I",
    "TAXA_DEFASAGEM_II",
    "TIPO_CALCULO_AVANCADOS",
    "TIPO_CALCULO_DEFASAGEM_I",
    "TIPO_CALCULO_DEFASAGEM_II"
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

mapeamento_faixas_etarias_indicadores <- function() {
  data.frame(
    FAIXA_ETARIA = c(
      "0 a 3 anos",
      "4 a 5 anos",
      "6 a 10 anos",
      "11 a 14 anos",
      "15 a 17 anos",
      "18 a 19 anos"
    ),
    ETAPA_ENSINO_ADEQUADA = c("CRE", "PRE", "AI", "AF", "EM", "POS_EM"),
    ETAPA_ENSINO_ADEQUADA_NOME = c(
      "Creche",
      "Pre-escola",
      "Anos iniciais",
      "Anos finais",
      "Ensino medio",
      "Pos-ensino medio"
    ),
    ORDEM_FAIXA_ETARIA = seq_len(6),
    stringsAsFactors = FALSE
  )
}

numerador_taxa_bruta_matricula <- function(indicadores) {
  componentes <- indicadores[, c(
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II"
  )]

  componentes[is.na(componentes)] <- 0
  rowSums(componentes)
}

adicionar_matriculas_faixa_etaria <- function(indicadores, matriculas_faixaetaria) {
  if (is.null(matriculas_faixaetaria)) {
    indicadores$MAT_FAIXA_ETARIA <- rep(NA_real_, nrow(indicadores))
    return(indicadores)
  }

  faixas_etapa <- indicadores$FAIXA_ETARIA
  matriculas_faixaetaria <- matriculas_faixaetaria[
    matriculas_faixaetaria$FAIXA_ETARIA %in% faixas_etapa,
  ]

  matriculas_faixaetaria <- aggregate(
    QT_MAT ~ NO_UF + FAIXA_ETARIA + ANO,
    data = matriculas_faixaetaria,
    FUN = sum,
    na.rm = TRUE
  )
  names(matriculas_faixaetaria)[names(matriculas_faixaetaria) == "QT_MAT"] <- "MAT_FAIXA_ETARIA"

  indicadores <- merge(
    indicadores,
    matriculas_faixaetaria,
    by = c("NO_UF", "FAIXA_ETARIA", "ANO"),
    all.x = TRUE
  )

  if (anyNA(indicadores$MAT_FAIXA_ETARIA)) {
    stop("Ha combinacoes de UF, etapa e ano sem matriculas por faixa etaria.", call. = FALSE)
  }

  indicadores
}

adicionar_indicadores_fluxo_etario <- function(indicadores, matriculas) {
  mapeamento <- mapeamento_indicadores_fluxo_etario()

  indicadores <- adicionar_matriculas_fluxo_etario(
    indicadores = indicadores,
    matriculas = matriculas,
    mapeamento = mapeamento,
    indicador = "AVANCADOS"
  )
  indicadores <- adicionar_matriculas_fluxo_etario(
    indicadores = indicadores,
    matriculas = matriculas,
    mapeamento = mapeamento,
    indicador = "DEFASAGEM_I"
  )
  indicadores <- adicionar_matriculas_fluxo_etario(
    indicadores = indicadores,
    matriculas = matriculas,
    mapeamento = mapeamento,
    indicador = "DEFASAGEM_II"
  )

  indicadores
}

mapeamento_indicadores_fluxo_etario <- function() {
  data.frame(
    INDICADOR = c(
      rep("AVANCADOS", 4),
      rep("DEFASAGEM_I", 5),
      rep("DEFASAGEM_II", 3)
    ),
    ETAPA_ENSINO_ADEQUADA = c(
      "CRE", "PRE", "AI", "AF",
      "PRE", "AI", "AF", "EM", "POS_EM",
      "AI", "EM", "POS_EM"
    ),
    ETAPA_MATRICULA = c(
      "PRE", "AI", "AF", "EM",
      "CRE", "PRE", "AI", "AF", "EM",
      "CRE", "AI", "AF"
    ),
    FAIXA_ETARIA = c(
      "0 a 3 anos", "0 a 5 anos", "0 a 10 anos", "0 a 14 anos",
      "4 a 5 anos", "6 anos ou mais", "11 a 14 anos", "15 a 17 anos", "18 a 19 anos",
      "6 anos ou mais", "15 a 17 anos", "18 a 19 anos"
    ),
    TIPO_CALCULO = c(
      "exato", "aproximado", "aproximado", "aproximado",
      "exato", "aproximado", "exato", "exato", "exato",
      "aproximado", "exato", "exato"
    ),
    stringsAsFactors = FALSE
  )
}

adicionar_matriculas_fluxo_etario <- function(indicadores, matriculas, mapeamento, indicador) {
  mapeamento <- mapeamento[mapeamento$INDICADOR == indicador, ]
  col_matriculas <- paste0("MAT_", indicador)
  col_tipo <- paste0("TIPO_CALCULO_", indicador)

  indicadores[[col_matriculas]] <- rep(NA_real_, nrow(indicadores))
  indicadores[[col_tipo]] <- rep("nao_aplicavel", nrow(indicadores))

  for (i in seq_len(nrow(mapeamento))) {
    etapa_ensino <- mapeamento$ETAPA_ENSINO_ADEQUADA[i]
    etapa_matricula <- mapeamento$ETAPA_MATRICULA[i]
    faixa_etaria <- mapeamento$FAIXA_ETARIA[i]
    linhas <- indicadores$ETAPA_ENSINO_ADEQUADA == etapa_ensino

    if (!any(linhas)) {
      next
    }

    matriculas_indicador <- matriculas[
      matriculas$ETAPA_ENSINO == etapa_matricula &
        matriculas$FAIXA_ETARIA == faixa_etaria,
      c("NO_UF", "ANO", "QT_MAT")
    ]

    if (nrow(matriculas_indicador) == 0) {
      stop(
        sprintf(
          "Nao ha matriculas para calcular `%s` de `%s` com `%s` em `%s`.",
          col_matriculas,
          etapa_ensino,
          faixa_etaria,
          etapa_matricula
        ),
        call. = FALSE
      )
    }

    matriculas_indicador <- aggregate(
      QT_MAT ~ NO_UF + ANO,
      data = matriculas_indicador,
      FUN = sum,
      na.rm = TRUE
    )

    chaves_indicadores <- paste(indicadores$NO_UF[linhas], indicadores$ANO[linhas], sep = "\r")
    chaves_matriculas <- paste(matriculas_indicador$NO_UF, matriculas_indicador$ANO, sep = "\r")
    posicoes <- match(chaves_indicadores, chaves_matriculas)

    if (anyNA(posicoes)) {
      stop(
        sprintf("Ha combinacoes de UF e ano sem matriculas para calcular `%s`.", col_matriculas),
        call. = FALSE
      )
    }

    indicadores[[col_matriculas]][linhas] <- matriculas_indicador$QT_MAT[posicoes]
    indicadores[[col_tipo]][linhas] <- mapeamento$TIPO_CALCULO[i]
  }

  indicadores
}

taxa_matricula_opcional <- function(matriculas, populacao) {
  resultado <- rep(NA_real_, length(matriculas))
  presentes <- !is.na(matriculas)

  if (any(presentes)) {
    resultado[presentes] <- taxa_liquida_matricula(
      matriculas[presentes],
      populacao[presentes]
    )
  }

  resultado
}

percentual_matriculas_fora_faixa_opcional <- function(matriculas_fora_faixa, total_matriculas) {
  resultado <- rep(NA_real_, length(matriculas_fora_faixa))
  presentes <- !is.na(matriculas_fora_faixa) & !is.na(total_matriculas)

  if (any(presentes)) {
    resultado[presentes] <- percentual_matriculas_fora_faixa(
      matriculas_fora_faixa[presentes],
      total_matriculas[presentes]
    )
  }

  resultado
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
  faixas_com_etapa_regular <- indicadores$ETAPA_ENSINO_ADEQUADA != "POS_EM"

  if (anyNA(indicadores$MAT_FAIXA_ADEQUADA[faixas_com_etapa_regular])) {
    stop("Ha combinacoes de UF, etapa e ano sem matriculas na faixa adequada.", call. = FALSE)
  }

  if (anyNA(indicadores$TOTAL_MATRICULAS[faixas_com_etapa_regular])) {
    stop("Ha combinacoes de UF, etapa e ano sem total de matriculas.", call. = FALSE)
  }

  if (anyNA(indicadores$POP_FAIXA_ADEQUADA)) {
    stop("Ha combinacoes de UF, etapa e ano sem populacao correspondente.", call. = FALSE)
  }

  if (any(indicadores$MAT_FAIXA_ADEQUADA > indicadores$TOTAL_MATRICULAS, na.rm = TRUE)) {
    stop("Ha matriculas na faixa adequada maiores que o total de matriculas.", call. = FALSE)
  }

  invisible(TRUE)
}
