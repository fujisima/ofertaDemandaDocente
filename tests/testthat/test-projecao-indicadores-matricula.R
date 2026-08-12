test_that("projetar_indicadores_matricula aplica metas gerais e especificas", {
  indicadores_observados <- data.frame(
    ANO = c(2025, 2025, 2025),
    SIGLA_UF = c("AC", "SP", "AC"),
    NO_UF = c("Acre", "Sao Paulo", "Acre"),
    FAIXA_ETARIA = c("0 a 3 anos", "0 a 3 anos", "18 a 19 anos"),
    ETAPA_ENSINO = c("CRE", "CRE", "POS_EM"),
    ETAPA_ENSINO_NOME = c("Creche", "Creche", "Pos-ensino medio"),
    RZ_AVANCADOS = c(0.5, 0.5, NA),
    RZ_DEF_1 = c(0.3, 0.25, NA),
    RZ_DEF_2 = c(0.2, 0.25, NA),
    TAXA_BRUTA_MATRICULA = c(60, 70, 30),
    TAXA_LIQUIDA_MATRICULA = c(50, 60, NA)
  )
  populacao <- data.frame(
    SIGLA = c("AC", "AC", "SP", "SP", "AC", "AC"),
    LOCAL = c("Acre", "Acre", "Sao Paulo", "Sao Paulo", "Acre", "Acre"),
    ETAPA_ENSINO = c("CRE", "CRE", "CRE", "CRE", "POS_EM", "POS_EM"),
    ANO = c(2026, 2036, 2026, 2036, 2026, 2036),
    POPULACAO = c(1000, 1200, 2000, 2200, 300, 320)
  )
  metas <- rbind(
    criar_metas_indicadores_gerais(
      taxa_bruta_matricula = c(
        "0 a 3 anos" = 90,
        "18 a 19 anos" = 50
      ),
      taxa_liquida_matricula = c("0 a 3 anos" = 70),
      ano_target = 2036
    ),
    data.frame(
      SIGLA_UF = "SP",
      FAIXA_ETARIA = "0 a 3 anos",
      INDICADOR = "TAXA_LIQUIDA_MATRICULA",
      ANO_TARGET = 2036,
      TARGET = 80
    )
  )

  resultado <- projetar_indicadores_matricula(
    indicadores_observados = indicadores_observados,
    populacao = populacao,
    metas = metas,
    anos = c(2026, 2036)
  )

  expect_false(any(c("ETAPA_ENSINO", "ETAPA_ENSINO_NOME") %in% names(resultado)))

  ac_2026 <- resultado[
    resultado$SIGLA_UF == "AC" &
      resultado$FAIXA_ETARIA == "0 a 3 anos" &
      resultado$ANO == 2026,
  ]
  ac_2036 <- resultado[
    resultado$SIGLA_UF == "AC" &
      resultado$FAIXA_ETARIA == "0 a 3 anos" &
      resultado$ANO == 2036,
  ]
  sp_2036 <- resultado[
    resultado$SIGLA_UF == "SP" &
      resultado$FAIXA_ETARIA == "0 a 3 anos" &
      resultado$ANO == 2036,
  ]
  ac_1819_2036 <- resultado[
    resultado$SIGLA_UF == "AC" &
      resultado$FAIXA_ETARIA == "18 a 19 anos" &
      resultado$ANO == 2036,
  ]

  expect_equal(ac_2026$TAXA_BRUTA_MATRICULA, 60 + (90 - 60) / 11, tolerance = 1e-10)
  expect_equal(ac_2026$TAXA_LIQUIDA_MATRICULA, 50 + (70 - 50) / 11, tolerance = 1e-10)
  expect_equal(ac_2036$TAXA_BRUTA_MATRICULA, 90, tolerance = 1e-10)
  expect_equal(ac_2036$TAXA_LIQUIDA_MATRICULA, 70, tolerance = 1e-10)
  expect_equal(sp_2036$TAXA_BRUTA_MATRICULA, 90, tolerance = 1e-10)
  expect_equal(sp_2036$TAXA_LIQUIDA_MATRICULA, 80, tolerance = 1e-10)
  expect_equal(ac_2036$TOTAL_MATRICULAS, 90 / 100 * 1200, tolerance = 1e-10)
  expect_equal(ac_2036$MAT_FAIXA_ADEQUADA, 70 / 100 * 1200, tolerance = 1e-10)
  expect_equal(ac_2036$MAT_AVANCADOS, (90 - 70) * 0.5 / 100 * 1200, tolerance = 1e-10)
  expect_equal(ac_2036$MAT_DEFASAGEM_I, (90 - 70) * 0.3 / 100 * 1200, tolerance = 1e-10)
  expect_equal(ac_2036$MAT_DEFASAGEM_II, (90 - 70) * 0.2 / 100 * 1200, tolerance = 1e-10)
  expect_equal(ac_1819_2036$TAXA_BRUTA_MATRICULA, 50, tolerance = 1e-10)
  expect_equal(ac_1819_2036$TAXA_LIQUIDA_MATRICULA, 0, tolerance = 1e-10)
  expect_equal(ac_1819_2036$MAT_FAIXA_ADEQUADA, 0, tolerance = 1e-10)
  expect_equal(ac_1819_2036$MAT_AVANCADOS, 0, tolerance = 1e-10)
  expect_equal(ac_1819_2036$MAT_DEFASAGEM_I, 0, tolerance = 1e-10)
  expect_equal(ac_1819_2036$MAT_DEFASAGEM_II, 0, tolerance = 1e-10)
})

test_that("projetar_indicadores_matricula exige metas para todos os indicadores", {
  indicadores_observados <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    FAIXA_ETARIA = "0 a 3 anos",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    RZ_AVANCADOS = 0.5,
    RZ_DEF_1 = 0.3,
    RZ_DEF_2 = 0.2,
    TAXA_BRUTA_MATRICULA = 60,
    TAXA_LIQUIDA_MATRICULA = 50
  )
  populacao <- data.frame(
    SIGLA = "AC",
    LOCAL = "Acre",
    ETAPA_ENSINO = "CRE",
    ANO = 2026,
    POPULACAO = 1000
  )
  metas <- data.frame(
    SIGLA_UF = NA_character_,
    FAIXA_ETARIA = "0 a 3 anos",
    INDICADOR = "TAXA_BRUTA_MATRICULA",
    ANO_TARGET = 2036,
    TARGET = 90
  )

  expect_error(
    projetar_indicadores_matricula(indicadores_observados, populacao, metas, anos = 2026),
    "Ha combinacoes sem meta para `TAXA_LIQUIDA_MATRICULA`",
    fixed = TRUE
  )
})

test_that("criar_metas_indicadores_gerais rejeita taxa liquida para 18 a 19 anos", {
  expect_error(
    criar_metas_indicadores_gerais(
      taxa_bruta_matricula = c("18 a 19 anos" = 50),
      taxa_liquida_matricula = c("18 a 19 anos" = 30)
    ),
    "A faixa `18 a 19 anos` nao deve ter meta de `taxa_liquida_matricula`",
    fixed = TRUE
  )
})
