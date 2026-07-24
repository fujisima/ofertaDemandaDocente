test_that("projetar_indicadores_matricula aplica metas gerais e especificas", {
  indicadores_observados <- data.frame(
    ANO = c(2025, 2025),
    SIGLA_UF = c("AC", "SP"),
    NO_UF = c("Acre", "Sao Paulo"),
    ETAPA_ENSINO = c("CRE", "CRE"),
    ETAPA_ENSINO_NOME = c("Creche", "Creche"),
    TAXA_LIQUIDA_MATRICULA = c(50, 60),
    PERCENTUAL_MATRICULAS_FORA_FAIXA = c(20, 15)
  )
  populacao <- data.frame(
    SIGLA = c("AC", "AC", "SP", "SP"),
    LOCAL = c("Acre", "Acre", "Sao Paulo", "Sao Paulo"),
    ETAPA_ENSINO = "CRE",
    ANO = c(2026, 2036, 2026, 2036),
    POPULACAO = c(1000, 1200, 2000, 2200)
  )
  metas <- rbind(
    criar_metas_indicadores_gerais(
      taxa_liquida_matricula = c(CRE = 70),
      percentual_matriculas_fora_faixa = c(CRE = 10),
      ano_target = 2036
    ),
    data.frame(
      SIGLA_UF = "SP",
      ETAPA_ENSINO = "CRE",
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

  ac_2036 <- resultado[resultado$SIGLA_UF == "AC" & resultado$ANO == 2036, ]
  sp_2036 <- resultado[resultado$SIGLA_UF == "SP" & resultado$ANO == 2036, ]

  expect_equal(ac_2036$TAXA_LIQUIDA_MATRICULA, 70, tolerance = 1e-10)
  expect_equal(ac_2036$PERCENTUAL_MATRICULAS_FORA_FAIXA, 10, tolerance = 1e-10)
  expect_equal(sp_2036$TAXA_LIQUIDA_MATRICULA, 80, tolerance = 1e-10)
  expect_equal(sp_2036$PERCENTUAL_MATRICULAS_FORA_FAIXA, 10, tolerance = 1e-10)
  expect_equal(ac_2036$TOTAL_MATRICULAS, (70 / 100 * 1200) / (1 - 10 / 100), tolerance = 1e-10)
})

test_that("projetar_indicadores_matricula exige metas para todos os indicadores", {
  indicadores_observados <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    TAXA_LIQUIDA_MATRICULA = 50,
    PERCENTUAL_MATRICULAS_FORA_FAIXA = 20
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
    ETAPA_ENSINO = "CRE",
    INDICADOR = "TAXA_LIQUIDA_MATRICULA",
    ANO_TARGET = 2036,
    TARGET = 70
  )

  expect_error(
    projetar_indicadores_matricula(indicadores_observados, populacao, metas, anos = 2026),
    "Ha combinacoes sem meta para `PERCENTUAL_MATRICULAS_FORA_FAIXA`",
    fixed = TRUE
  )
})
