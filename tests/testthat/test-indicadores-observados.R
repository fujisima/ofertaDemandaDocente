test_that("calcular_indicadores_matricula_observados calcula indicadores por UF, etapa e ano", {
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
  expect_equal(resultado$ETAPA_ENSINO, rep(c("AF", "AI", "CRE", "EM", "PRE"), times = 2))
  expect_equal(resultado$MAT_FORA_FAIXA, c(70, 50, 30, 70, 20, 80, 50, 30, 80, 20))
  expect_equal(resultado$MAT_FAIXA_ETARIA, c(360, 260, 150, 450, 210, 380, 280, 180, 480, 230))
  expect_equal(resultado$TAXA_LIQUIDA_MATRICULA, c(55, 50, 7, 430 / 7, 60, 350 / 5.5, 60, 10, 460 / 6.5, 80))
  expect_equal(resultado$TAXA_BRUTA_MATRICULA, c(60, 52, 15, 450 / 7, 70, 380 / 5.5, 280 / 4.5, 20, 480 / 6.5, 92))
  expect_equal(resultado$PERCENTUAL_MATRICULAS_FORA_FAIXA, c(17.5, 50 / 300 * 100, 30, 14, 10, 80 / 430 * 100, 50 / 320 * 100, 25, 80 / 540 * 100, 20 / 220 * 100))
  expect_equal(resultado$MAT_AVANCADOS, c(30, 20, 5, NA, 12, 31, 21, 6, NA, 13))
  expect_equal(resultado$MAT_DEFASAGEM_I, c(14, 7, NA, 4, 8, 15, 8, NA, 5, 10))
  expect_equal(resultado$MAT_DEFASAGEM_II, c(7, 2, NA, 3, NA, 8, 3, NA, 4, NA))
  expect_equal(resultado$TAXA_AVANCADOS, c(30 / 600, 20 / 500, 5 / 1000, NA, 12 / 300, 31 / 550, 21 / 450, 6 / 900, NA, 13 / 250) * 100)
  expect_equal(resultado$TAXA_DEFASAGEM_I, c(14 / 600, 7 / 500, NA, 4 / 700, 8 / 300, 15 / 550, 8 / 450, NA, 5 / 650, 10 / 250) * 100)
  expect_equal(resultado$TAXA_DEFASAGEM_II, c(7 / 600, 2 / 500, NA, 3 / 700, NA, 8 / 550, 3 / 450, NA, 4 / 650, NA) * 100)
  expect_equal(resultado$TIPO_CALCULO_AVANCADOS, c("aproximado", "aproximado", "exato", "nao_aplicavel", "aproximado", "aproximado", "aproximado", "exato", "nao_aplicavel", "aproximado"))
  expect_equal(resultado$TIPO_CALCULO_DEFASAGEM_I, c("exato", "aproximado", "nao_aplicavel", "exato", "exato", "exato", "aproximado", "nao_aplicavel", "exato", "exato"))
  expect_equal(resultado$TIPO_CALCULO_DEFASAGEM_II, c("aproximado", "aproximado", "nao_aplicavel", "exato", "nao_aplicavel", "aproximado", "aproximado", "nao_aplicavel", "exato", "nao_aplicavel"))
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
