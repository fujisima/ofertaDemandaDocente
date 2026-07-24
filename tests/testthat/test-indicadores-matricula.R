test_that("taxa_liquida_matricula calcula indicador percentual", {
  resultado <- taxa_liquida_matricula(
    matriculas_faixa_adequada = c(700, 450),
    populacao_faixa_adequada = c(1000, 500)
  )

  expect_equal(resultado, c(70, 90))
})

test_that("percentual_matriculas_fora_faixa calcula indicador percentual", {
  resultado <- percentual_matriculas_fora_faixa(
    matriculas_fora_faixa = c(30, 125),
    total_matriculas = c(100, 500)
  )

  expect_equal(resultado, c(30, 25))
})

test_that("indicadores de matricula funcionam por UF, etapa e ano", {
  dados <- data.frame(
    NO_UF = c("Acre", "Acre", "Sao Paulo"),
    ETAPA_ENSINO = c("CRE", "PRE", "CRE"),
    ANO = c(2024, 2024, 2024),
    MAT_FAIXA_ADEQUADA = c(700, 450, 900),
    POP_FAIXA_ADEQUADA = c(1000, 500, 1000),
    MAT_FORA_FAIXA = c(30, 125, 80),
    TOTAL_MATRICULAS = c(730, 575, 980)
  )

  dados$TAXA_LIQUIDA_MATRICULA <- taxa_liquida_matricula(
    dados$MAT_FAIXA_ADEQUADA,
    dados$POP_FAIXA_ADEQUADA
  )
  dados$PERCENTUAL_MATRICULAS_FORA_FAIXA <- percentual_matriculas_fora_faixa(
    dados$MAT_FORA_FAIXA,
    dados$TOTAL_MATRICULAS
  )

  expect_equal(dados$TAXA_LIQUIDA_MATRICULA, c(70, 90, 90))
  expect_equal(
    dados$PERCENTUAL_MATRICULAS_FORA_FAIXA,
    c(30 / 730, 125 / 575, 80 / 980) * 100
  )
})

test_that("indicadores de matricula validam denominadores", {
  expect_error(
    taxa_liquida_matricula(10, 0),
    "`populacao_faixa_adequada` deve ser maior que zero",
    fixed = TRUE
  )

  expect_error(
    percentual_matriculas_fora_faixa(10, 0),
    "`total_matriculas` deve ser maior que zero",
    fixed = TRUE
  )
})
