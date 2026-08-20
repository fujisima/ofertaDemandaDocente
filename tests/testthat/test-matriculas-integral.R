test_that("percentuais de turno sao calculados por etapa e localizacao", {
  matriculas_turno <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = c("1", "2"),
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    ETAPA_ENSINO_DETALHE = "CRE",
    ETAPA_ENSINO_DETALHE_NOME = "Creche",
    QT_MAT_INTEGRAL = c(30, 20),
    QT_MAT_PARCIAL = c(70, 80)
  )
  matriculas_em_integral <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = rep(c("1", "2"), 2),
    ETAPA_ENSINO = "EM",
    ETAPA_ENSINO_NOME = "Ensino medio",
    ETAPA_ENSINO_DETALHE = rep(c("EM_PROP", "EM_EPT"), each = 2),
    ETAPA_ENSINO_DETALHE_NOME = rep(
      c("Ensino medio propedeutico", "Ensino medio EPT"),
      each = 2
    ),
    QT_MAT_INTEGRAL = c(40, 30, 15, 10)
  )
  totais_em <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = c("Capital", "Interior"),
    QT_MAT_EM_PROPEDEUTICO = c(100, 60),
    QT_MAT_EM_EPT = c(30, 20)
  )
  municipios_capital_rm <- data.frame(COD_MUN = "1")

  resultado <- calcular_percentuais_matriculas_integral_localizacao(
    matriculas_turno_municipio = matriculas_turno,
    matriculas_em_integral = matriculas_em_integral,
    totais_em_localizacao = totais_em,
    municipios_capital_rm = municipios_capital_rm
  )

  expect_equal(nrow(resultado), 6)
  expect_equal(
    resultado$GRUPO_LOCALIZACAO,
    rep(c("Capital", "Interior"), 3)
  )
  expect_equal(
    resultado$P_INTEGRAL,
    c(0.3, 0.2, 0.4, 0.5, 0.5, 0.5)
  )
  expect_equal(resultado$P_INTEGRAL + resultado$P_PARCIAL, rep(1, 6))
})

test_that("integral do ensino medio nao pode exceder o total", {
  matriculas_turno <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = "1",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    ETAPA_ENSINO_DETALHE = "CRE",
    ETAPA_ENSINO_DETALHE_NOME = "Creche",
    QT_MAT_INTEGRAL = 0,
    QT_MAT_PARCIAL = 0
  )
  matriculas_em_integral <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = "1",
    ETAPA_ENSINO = "EM",
    ETAPA_ENSINO_NOME = "Ensino medio",
    ETAPA_ENSINO_DETALHE = "EM_PROP",
    ETAPA_ENSINO_DETALHE_NOME = "Ensino medio propedeutico",
    QT_MAT_INTEGRAL = 101
  )
  totais_em <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = "Capital",
    QT_MAT_EM_PROPEDEUTICO = 100,
    QT_MAT_EM_EPT = 0
  )

  expect_error(
    calcular_percentuais_matriculas_integral_localizacao(
      matriculas_turno_municipio = matriculas_turno,
      matriculas_em_integral = matriculas_em_integral,
      totais_em_localizacao = totais_em,
      municipios_capital_rm = data.frame(COD_MUN = "1")
    ),
    "devem ser nao negativas"
  )
})
