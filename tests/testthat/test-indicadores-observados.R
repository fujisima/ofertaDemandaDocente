test_that("calcular_indicadores_matricula_observados calcula indicadores por UF, etapa e ano", {
  matriculas <- data.frame(
    ANO = c(2016, 2016, 2016, 2016, 2025, 2025, 2025, 2025),
    NO_UF = "Acre",
    ETAPA_ENSINO = c("CRE", "CRE", "PRE", "PRE", "CRE", "CRE", "PRE", "PRE"),
    ETAPA_ENSINO_NOME = c("Creche", "Creche", "Pre-escola", "Pre-escola", "Creche", "Creche", "Pre-escola", "Pre-escola"),
    FAIXA_ETARIA = c("Total", "0 a 3 anos", "Total", "4 a 5 anos", "Total", "0 a 3 anos", "Total", "4 a 5 anos"),
    QT_MAT = c(100, 70, 200, 180, 120, 90, 220, 200)
  )
  populacao <- data.frame(
    SIGLA = "AC",
    LOCAL = "Acre",
    ETAPA_ENSINO = c("CRE", "PRE", "CRE", "PRE"),
    ANO = c(2016, 2016, 2025, 2025),
    POPULACAO = c(1000, 300, 900, 250)
  )
  matriculas_faixaetaria <- data.frame(
    ANO = c(2016, 2016, 2025, 2025),
    NO_UF = "Acre",
    FAIXA_ETARIA = c("0 a 3 anos", "4 a 5 anos", "0 a 3 anos", "4 a 5 anos"),
    QT_MAT = c(150, 210, 180, 230)
  )

  resultado <- calcular_indicadores_matricula_observados(
    matriculas = matriculas,
    populacao = populacao,
    matriculas_faixaetaria = matriculas_faixaetaria
  )

  expect_equal(nrow(resultado), 4)
  expect_equal(resultado$ANO, c(2016, 2016, 2025, 2025))
  expect_equal(resultado$MAT_FORA_FAIXA, c(30, 20, 30, 20))
  expect_equal(resultado$MAT_FAIXA_ETARIA, c(150, 210, 180, 230))
  expect_equal(resultado$TAXA_LIQUIDA_MATRICULA, c(7, 60, 10, 80))
  expect_equal(resultado$TAXA_BRUTA_MATRICULA, c(15, 70, 20, 92))
  expect_equal(resultado$PERCENTUAL_MATRICULAS_FORA_FAIXA, c(30, 10, 25, 20 / 220 * 100))
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
