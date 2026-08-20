test_that("percentuais do ensino medio sao calculados por capital e interior", {
  matriculas <- data.frame(
    ANO = rep(2025, 4),
    NO_UF = rep(c("Acre", "Sao Paulo"), each = 2),
    COD_MUN = c("1200401", "1200000", "3550308", "3500000"),
    QT_MAT_EM_PROPEDEUTICO = c(80, 60, 30, 10),
    QT_MAT_EM_EPT = c(20, 40, 70, 10)
  )
  municipios_capital_rm <- data.frame(
    COD_MUN = c("1200401", "3550308")
  )

  resultado <- calcular_percentuais_em_localizacao(
    matriculas,
    municipios_capital_rm
  )

  expect_equal(nrow(resultado), 4)
  expect_equal(
    resultado$GRUPO_LOCALIZACAO,
    rep(c("Capital", "Interior"), 2)
  )
  expect_equal(resultado$P_EM_PROPEDEUTICO, c(0.8, 0.6, 0.3, 0.5))
  expect_equal(resultado$P_EM_EPT, c(0.2, 0.4, 0.7, 0.5))
  expect_equal(
    resultado$P_EM_PROPEDEUTICO + resultado$P_EM_EPT,
    rep(1, 4)
  )
})

test_that("grupo sem municipios tem totais zero e percentuais ausentes", {
  matriculas <- data.frame(
    ANO = 2025,
    NO_UF = "Distrito Federal",
    COD_MUN = "5300108",
    QT_MAT_EM_PROPEDEUTICO = 75,
    QT_MAT_EM_EPT = 25
  )
  municipios_capital_rm <- data.frame(COD_MUN = "5300108")

  resultado <- calcular_percentuais_em_localizacao(
    matriculas,
    municipios_capital_rm
  )

  expect_equal(resultado$GRUPO_LOCALIZACAO, c("Capital", "Interior"))
  expect_equal(resultado$QT_MAT_EM_TOTAL, c(100, 0))
  expect_equal(resultado$P_EM_PROPEDEUTICO[1], 0.75)
  expect_equal(resultado$P_EM_EPT[1], 0.25)
  expect_true(is.na(resultado$P_EM_PROPEDEUTICO[2]))
  expect_true(is.na(resultado$P_EM_EPT[2]))
})
