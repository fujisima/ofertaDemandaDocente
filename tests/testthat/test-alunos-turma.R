test_that("ATU por localizacao usa turmas estimadas como ponderador", {
  atu <- data.frame(
    ANO = rep(2025, 4),
    SIGLA_UF = rep("AC", 4),
    COD_MUN = c("1", "2", "3", "1"),
    ETAPA_ENSINO = c("CRE", "CRE", "CRE", "EM"),
    MEDIA_ALUNOS_TURMA = c(20, 15, 10, 25)
  )
  matriculas_turno <- data.frame(
    ANO = rep(2025, 3),
    NO_UF = rep("Acre", 3),
    COD_MUN = c("1", "2", "3"),
    ETAPA_ENSINO = rep("CRE", 3),
    QT_MAT_INTEGRAL = c(40, 10, 20),
    QT_MAT_PARCIAL = c(60, 50, 20)
  )
  matriculas_em <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = "1",
    QT_MAT_EM_TOTAL = 100,
    QT_MAT_EM_PROPEDEUTICO = 80,
    QT_MAT_EM_EPT = 20
  )

  resultado <- calcular_media_alunos_turma_localizacao(
    atu_municipio = atu,
    matriculas_turno_municipio = matriculas_turno,
    matriculas_em_municipio = matriculas_em,
    municipios_capital_rm = data.frame(COD_MUN = c("1", "2"))
  )

  cre_capital <- resultado[
    resultado$GRUPO_LOCALIZACAO == "Capital" &
      resultado$ETAPA_ENSINO == "CRE",
  ]
  cre_interior <- resultado[
    resultado$GRUPO_LOCALIZACAO == "Interior" &
      resultado$ETAPA_ENSINO == "CRE",
  ]

  expect_equal(nrow(resultado), 10)
  expect_equal(cre_capital$QT_MAT_TOTAL, 160)
  expect_equal(cre_capital$QT_TURMAS_ESTIMADAS, 9)
  expect_equal(cre_capital$MEDIA_ALUNOS_TURMA, 160 / 9)
  expect_equal(cre_interior$MEDIA_ALUNOS_TURMA, 10)
})

test_that("matriculas sem ATU exigem referencia observada no grupo", {
  atu <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    COD_MUN = "1",
    ETAPA_ENSINO = "CRE",
    MEDIA_ALUNOS_TURMA = NA_real_
  )
  matriculas_turno <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    COD_MUN = "1",
    ETAPA_ENSINO = "CRE",
    QT_MAT_INTEGRAL = 10,
    QT_MAT_PARCIAL = 0
  )
  matriculas_em <- data.frame(
    ANO = integer(),
    NO_UF = character(),
    COD_MUN = character(),
    QT_MAT_EM_TOTAL = numeric(),
    QT_MAT_EM_PROPEDEUTICO = numeric(),
    QT_MAT_EM_EPT = numeric()
  )

  expect_error(
    calcular_media_alunos_turma_localizacao(
      atu_municipio = atu,
      matriculas_turno_municipio = matriculas_turno,
      matriculas_em_municipio = matriculas_em,
      municipios_capital_rm = data.frame(COD_MUN = "1")
    ),
    "sem ATU municipal ou referencia do grupo"
  )
})

test_that("ATU ausente e imputado pela media ponderada do grupo", {
  atu <- data.frame(
    ANO = rep(2025, 2),
    SIGLA_UF = rep("SP", 2),
    COD_MUN = c("1", "2"),
    ETAPA_ENSINO = rep("CRE", 2),
    MEDIA_ALUNOS_TURMA = c(20, NA)
  )
  matriculas_turno <- data.frame(
    ANO = rep(2025, 2),
    NO_UF = rep("Sao Paulo", 2),
    COD_MUN = c("1", "2"),
    ETAPA_ENSINO = rep("CRE", 2),
    QT_MAT_INTEGRAL = c(0, 0),
    QT_MAT_PARCIAL = c(100, 4)
  )
  matriculas_em <- data.frame(
    ANO = integer(),
    NO_UF = character(),
    COD_MUN = character(),
    QT_MAT_EM_TOTAL = numeric()
  )

  resultado <- calcular_media_alunos_turma_localizacao(
    atu_municipio = atu,
    matriculas_turno_municipio = matriculas_turno,
    matriculas_em_municipio = matriculas_em,
    municipios_capital_rm = data.frame(COD_MUN = c("1", "2"))
  )
  cre_capital <- resultado[
    resultado$GRUPO_LOCALIZACAO == "Capital" &
      resultado$ETAPA_ENSINO == "CRE",
  ]

  expect_equal(cre_capital$QT_MAT_TOTAL, 104)
  expect_equal(cre_capital$QT_TURMAS_ESTIMADAS, 5.2)
  expect_equal(cre_capital$QT_MAT_ATU_IMPUTADO, 4)
  expect_equal(cre_capital$QT_MUNICIPIOS_ATU_IMPUTADO, 1)
  expect_equal(cre_capital$MEDIA_ALUNOS_TURMA, 20)
})
