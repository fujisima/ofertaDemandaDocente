test_that("compor_matriculas_etapa_projetadas agrega componentes por etapa", {
  indicadores_matricula_projetados <- data.frame(
    ANO = rep(2036, 6),
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    FAIXA_ETARIA = c(
      "0 a 3 anos",
      "4 a 5 anos",
      "6 a 10 anos",
      "11 a 14 anos",
      "15 a 17 anos",
      "18 a 19 anos"
    ),
    TOTAL_MATRICULAS = c(100, 110, 120, 130, 140, 150),
    MAT_FAIXA_ADEQUADA = c(70, 80, 90, 100, 110, NA),
    MAT_AVANCADOS = c(5, 6, 7, 8, NA, NA),
    MAT_DEFASAGEM_I = c(NA, 9, 10, 11, 12, 13),
    MAT_DEFASAGEM_II = c(NA, NA, 14, NA, 15, 16),
    MAT_FORA_FAIXA = c(30, 30, 30, 30, 30, NA),
    POP_FAIXA_ADEQUADA = c(100, 100, 100, 100, 100, 100),
    TAXA_BRUTA_MATRICULA = c(100, 110, 120, 130, 140, 150),
    TAXA_LIQUIDA_MATRICULA = c(70, 80, 90, 100, 110, NA),
    TIPO_DADO = "projetado",
    stringsAsFactors = FALSE
  )

  resultado <- compor_matriculas_etapa_projetadas(indicadores_matricula_projetados)

  expect_equal(resultado$ETAPA_ENSINO, c("CRE", "PRE", "AI", "AF", "EM"))
  expect_equal(resultado$TOTAL_MATRICULAS, c(93, 95, 122, 135, 131))
  expect_false(any(c(
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II"
  ) %in% names(resultado)))
})
