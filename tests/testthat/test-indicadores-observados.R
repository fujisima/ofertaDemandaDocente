test_that("calcular_indicadores_matricula_observados calcula indicadores por UF, faixa etaria e ano", {
  matriculas <- data.frame(
    ANO = rep(c(2016, 2025), each = 24),
    NO_UF = "Acre",
    ETAPA_ENSINO = rep(c(
      "CRE", "CRE", "CRE", "CRE",
      "PRE", "PRE", "PRE", "PRE", "PRE",
      "AI", "AI", "AI", "AI", "AI", "AI",
      "AF", "AF", "AF", "AF", "AF",
      "EM", "EM", "EM", "EM"
    ), times = 2),
    ETAPA_ENSINO_NOME = rep(c(
      rep("Creche", 4),
      rep("Pre-escola", 5),
      rep("Anos iniciais", 6),
      rep("Anos finais", 5),
      rep("Ensino medio", 4)
    ), times = 2),
    FAIXA_ETARIA = rep(c(
      "Total", "0 a 3 anos", "4 a 5 anos", "6 anos ou mais",
      "Total", "0 a 3 anos", "4 a 5 anos", "6 anos ou mais", "6 a 10 anos",
      "Total", "0 a 5 anos", "6 a 10 anos", "11 a 14 anos", "15 a 17 anos", "6 anos ou mais",
      "Total", "0 a 10 anos", "11 a 14 anos", "15 a 17 anos", "6 a 10 anos",
      "Total", "0 a 14 anos", "15 a 17 anos", "11 a 14 anos"
    ), times = 2),
    QT_MAT = c(
      100, 70, 8, 2,
      200, 5, 180, 7, 1,
      300, 12, 250, 14, 3, 0,
      400, 20, 330, 4, 9,
      500, 30, 430, 11,
      120, 90, 10, 3,
      220, 6, 200, 8, 2,
      320, 13, 270, 15, 4, 0,
      430, 21, 350, 5, 10,
      540, 31, 460, 12
    )
  )
  populacao <- data.frame(
    SIGLA = "AC",
    LOCAL = "Acre",
    ETAPA_ENSINO = rep(c("CRE", "PRE", "AI", "AF", "EM"), times = 2),
    ANO = rep(c(2016, 2025), each = 5),
    POPULACAO = c(1000, 300, 500, 600, 700, 900, 250, 450, 550, 650)
  )
  matriculas_faixaetaria <- data.frame(
    ANO = rep(c(2016, 2025), each = 5),
    NO_UF = "Acre",
    FAIXA_ETARIA = rep(c("0 a 3 anos", "4 a 5 anos", "6 a 10 anos", "11 a 14 anos", "15 a 17 anos"), times = 2),
    QT_MAT = c(150, 210, 260, 360, 450, 180, 230, 280, 380, 480)
  )

  resultado <- calcular_indicadores_matricula_observados(
    matriculas = matriculas,
    populacao = populacao,
    matriculas_faixaetaria = matriculas_faixaetaria
  )

  expect_equal(nrow(resultado), 10)
  expect_equal(resultado$ANO, rep(c(2016, 2025), each = 5))
  expect_equal(resultado$FAIXA_ETARIA, rep(c("0 a 3 anos", "4 a 5 anos", "6 a 10 anos", "11 a 14 anos", "15 a 17 anos"), times = 2))
  expect_equal(resultado$ETAPA_ENSINO_ADEQUADA, rep(c("CRE", "PRE", "AI", "AF", "EM"), times = 2))
  expect_equal(resultado$MAT_FORA_FAIXA, c(30, 20, 50, 70, 70, 30, 20, 50, 80, 80))
  expect_equal(resultado$MAT_FAIXA_ETARIA, c(150, 210, 260, 360, 450, 180, 230, 280, 380, 480))
  expect_equal(resultado$MAT_TAXA_BRUTA_MATRICULA, c(75, 200, 279, 374, 437, 96, 223, 302, 396, 469))
  expect_equal(resultado$TAXA_LIQUIDA_MATRICULA, c(7, 60, 50, 55, 430 / 7, 10, 80, 60, 350 / 5.5, 460 / 6.5))
  expect_equal(resultado$TAXA_BRUTA_MATRICULA, c(7.5, 200 / 3, 279 / 5, 374 / 6, 437 / 7, 96 / 9, 89.2, 302 / 4.5, 396 / 5.5, 469 / 6.5))
  expect_equal(resultado$PERCENTUAL_MATRICULAS_FORA_FAIXA, c(30, 10, 50 / 300 * 100, 17.5, 14, 25, 20 / 220 * 100, 50 / 320 * 100, 80 / 430 * 100, 80 / 540 * 100))
  expect_equal(resultado$MAT_AVANCADOS, c(5, 12, 20, 30, NA, 6, 13, 21, 31, NA))
  expect_equal(resultado$MAT_DEFASAGEM_I, c(NA, 8, 7, 14, 4, NA, 10, 8, 15, 5))
  expect_equal(resultado$MAT_DEFASAGEM_II, c(NA, NA, 2, NA, 3, NA, NA, 3, NA, 4))
  expect_equal(resultado$TAXA_AVANCADOS, c(5 / 1000, 12 / 300, 20 / 500, 30 / 600, NA, 6 / 900, 13 / 250, 21 / 450, 31 / 550, NA) * 100)
  expect_equal(resultado$TAXA_DEFASAGEM_I, c(NA, 8 / 300, 7 / 500, 14 / 600, 4 / 700, NA, 10 / 250, 8 / 450, 15 / 550, 5 / 650) * 100)
  expect_equal(resultado$TAXA_DEFASAGEM_II, c(NA, NA, 2 / 500, NA, 3 / 700, NA, NA, 3 / 450, NA, 4 / 650) * 100)
  diferenca_taxa_bruta_liquida <- resultado$TAXA_BRUTA_MATRICULA - resultado$TAXA_LIQUIDA_MATRICULA
  expect_equal(resultado$RZ_AVANCADOS, resultado$TAXA_AVANCADOS / diferenca_taxa_bruta_liquida)
  expect_equal(resultado$RZ_DEF_1, resultado$TAXA_DEFASAGEM_I / diferenca_taxa_bruta_liquida)
  expect_equal(resultado$RZ_DEF_2, resultado$TAXA_DEFASAGEM_II / diferenca_taxa_bruta_liquida)
  expect_equal(resultado$TIPO_CALCULO_AVANCADOS, c("exato", "aproximado", "aproximado", "aproximado", "nao_aplicavel", "exato", "aproximado", "aproximado", "aproximado", "nao_aplicavel"))
  expect_equal(resultado$TIPO_CALCULO_DEFASAGEM_I, c("nao_aplicavel", "exato", "aproximado", "exato", "exato", "nao_aplicavel", "exato", "aproximado", "exato", "exato"))
  expect_equal(resultado$TIPO_CALCULO_DEFASAGEM_II, c("nao_aplicavel", "nao_aplicavel", "aproximado", "nao_aplicavel", "exato", "nao_aplicavel", "nao_aplicavel", "aproximado", "nao_aplicavel", "exato"))
  expect_equal(
    diferenca_taxa_bruta_liquida,
    rowSums(
      data.frame(
        avancados = ifelse(is.na(resultado$TAXA_AVANCADOS), 0, resultado$TAXA_AVANCADOS),
        defasagem_i = ifelse(is.na(resultado$TAXA_DEFASAGEM_I), 0, resultado$TAXA_DEFASAGEM_I),
        defasagem_ii = ifelse(is.na(resultado$TAXA_DEFASAGEM_II), 0, resultado$TAXA_DEFASAGEM_II)
      )
    )
  )
})

test_that("calcular_indicadores_matricula_observados valida populacao faltante", {
  matriculas <- data.frame(
    ANO = c(2016, 2016),
    NO_UF = "Acre",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    FAIXA_ETARIA = c("Total", "0 a 3 anos"),
    QT_MAT = c(100, 70)
  )
  populacao <- data.frame(
    SIGLA = character(),
    LOCAL = character(),
    ETAPA_ENSINO = character(),
    ANO = integer(),
    POPULACAO = numeric()
  )

  expect_error(
    calcular_indicadores_matricula_observados(matriculas, populacao),
    "Ha combinacoes de UF, etapa e ano sem populacao correspondente",
    fixed = TRUE
  )
})

test_that("mapeamento de fluxo etario evita dupla contagem em defasagem II", {
  mapeamento <- mapeamento_indicadores_fluxo_etario()

  expect_false(any(
    mapeamento$INDICADOR == "DEFASAGEM_II" &
      mapeamento$ETAPA_ENSINO_ADEQUADA == "AF"
  ))

  defasagem_ii_ai <- mapeamento[
    mapeamento$INDICADOR == "DEFASAGEM_II" &
      mapeamento$ETAPA_ENSINO_ADEQUADA == "AI",
    c("ETAPA_MATRICULA", "FAIXA_ETARIA", "TIPO_CALCULO")
  ]
  row.names(defasagem_ii_ai) <- NULL
  expect_equal(
    defasagem_ii_ai,
    data.frame(
      ETAPA_MATRICULA = "CRE",
      FAIXA_ETARIA = "6 anos ou mais",
      TIPO_CALCULO = "aproximado",
      stringsAsFactors = FALSE
    )
  )
  defasagem_pos_em <- mapeamento[
    mapeamento$ETAPA_ENSINO_ADEQUADA == "POS_EM",
    c("INDICADOR", "ETAPA_MATRICULA", "FAIXA_ETARIA", "TIPO_CALCULO")
  ]
  row.names(defasagem_pos_em) <- NULL
  expect_equal(
    defasagem_pos_em,
    data.frame(
      INDICADOR = c("DEFASAGEM_I", "DEFASAGEM_II"),
      ETAPA_MATRICULA = c("EM", "AF"),
      FAIXA_ETARIA = c("18 a 19 anos", "18 a 19 anos"),
      TIPO_CALCULO = c("exato", "exato"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("calcular_indicadores_matricula_observados usa taxa liquida zero em 18 a 19 anos", {
  matriculas <- data.frame(
    ANO = rep(2025, each = 25),
    NO_UF = "Acre",
    ETAPA_ENSINO = c(
      "CRE", "CRE", "CRE", "CRE",
      "PRE", "PRE", "PRE", "PRE", "PRE",
      "AI", "AI", "AI", "AI", "AI", "AI",
      "AF", "AF", "AF", "AF", "AF", "AF",
      "EM", "EM", "EM", "EM"
    ),
    ETAPA_ENSINO_NOME = c(
      rep("Creche", 4),
      rep("Pre-escola", 5),
      rep("Anos iniciais", 6),
      rep("Anos finais", 6),
      rep("Ensino medio", 4)
    ),
    FAIXA_ETARIA = c(
      "Total", "0 a 3 anos", "4 a 5 anos", "6 anos ou mais",
      "Total", "0 a 3 anos", "4 a 5 anos", "6 anos ou mais", "6 a 10 anos",
      "Total", "0 a 5 anos", "6 a 10 anos", "11 a 14 anos", "15 a 17 anos", "6 anos ou mais",
      "Total", "0 a 10 anos", "11 a 14 anos", "15 a 17 anos", "6 a 10 anos", "18 a 19 anos",
      "Total", "0 a 14 anos", "15 a 17 anos", "18 a 19 anos"
    ),
    QT_MAT = c(
      120, 90, 10, 3,
      220, 6, 200, 8, 2,
      320, 13, 270, 15, 4, 0,
      433, 21, 350, 5, 10, 3,
      540, 31, 460, 12
    ),
    stringsAsFactors = FALSE
  )
  populacao <- data.frame(
    SIGLA = "AC",
    LOCAL = "Acre",
    ETAPA_ENSINO = c("CRE", "PRE", "AI", "AF", "EM", "POS_EM"),
    ANO = 2025,
    POPULACAO = c(900, 250, 450, 550, 650, 700),
    stringsAsFactors = FALSE
  )

  resultado <- calcular_indicadores_matricula_observados(matriculas, populacao, anos = 2025)
  pos_em <- resultado[resultado$FAIXA_ETARIA == "18 a 19 anos", ]

  expect_equal(pos_em$TAXA_LIQUIDA_MATRICULA, 0)
  expect_equal(pos_em$TAXA_BRUTA_MATRICULA, 15 / 700 * 100)
  expect_true(is.na(pos_em$RZ_AVANCADOS))
  expect_equal(pos_em$RZ_DEF_1, 12 / 15)
  expect_equal(pos_em$RZ_DEF_2, 3 / 15)
})
