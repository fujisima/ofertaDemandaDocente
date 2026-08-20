test_that("metas de integral sao aplicadas conforme o percentual observado", {
  observados <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = "Capital",
    ETAPA_ENSINO = c("CRE", "EM", "EM"),
    ETAPA_ENSINO_NOME = c("Creche", "Ensino medio", "Ensino medio"),
    ETAPA_ENSINO_DETALHE = c("CRE", "EM_PROP", "EM_EPT"),
    ETAPA_ENSINO_DETALHE_NOME = c(
      "Creche",
      "Ensino medio propedeutico",
      "Ensino medio EPT"
    ),
    P_INTEGRAL = c(0.2, 0.5, 0.95),
    P_PARCIAL = c(0.8, 0.5, 0.05)
  )
  metas <- criar_metas_percentual_integral(
    percentual_integral = c(CRE = 50, EM_PROP = 50, EM_EPT = 50),
    incremento_acima_meta = 10,
    ano_target = 2036,
    limite = 100
  )

  resultado <- projetar_percentuais_matriculas_integral(
    percentuais_observados = observados,
    metas = metas,
    anos = c(2026, 2036),
    ano_base = 2025
  )
  resultado_2036 <- resultado[resultado$ANO == 2036, ]

  expect_equal(resultado_2036$P_INTEGRAL, c(0.5, 1, 0.6))
  expect_equal(
    resultado_2036$P_INTEGRAL + resultado_2036$P_PARCIAL,
    rep(1, 3)
  )
  expect_equal(
    resultado$P_INTEGRAL[
      resultado$ANO == 2026 & resultado$ETAPA_ENSINO_DETALHE == "CRE"
    ],
    0.2 + (0.5 - 0.2) / 11
  )
})

test_that("rateio por jornada preserva o total de matriculas", {
  matriculas <- data.frame(
    ANO = 2026,
    UF = 12,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    criterio_localizacao = "Capital",
    grupo_localizacao = "Capital",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_NOME = "Creche",
    ETAPA_ENSINO_DETALHE = "CRE",
    ETAPA_ENSINO_DETALHE_NOME = "Creche",
    P_COMPOSICAO = 1,
    ANO_REFERENCIA_COMPOSICAO = NA_integer_,
    FONTE_COMPOSICAO = NA_character_,
    TOTAL_MATRICULAS = 100
  )
  percentuais <- data.frame(
    ANO = 2026,
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = "Capital",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_DETALHE = "CRE",
    P_INTEGRAL = 0.4,
    P_PARCIAL = 0.6,
    P_INTEGRAL_BASE = 0.37,
    P_INTEGRAL_TARGET = 0.5,
    ANO_TARGET = 2036,
    META_PERCENTUAL_INTEGRAL = 50,
    INCREMENTO_ACIMA_META = 10,
    LIMITE_PERCENTUAL_INTEGRAL = 100
  )

  resultado <- ratear_matriculas_projetadas_por_jornada(
    matriculas_etapa_projetadas_localizacao = matriculas,
    percentuais_integral_projetados = percentuais,
    ano_referencia = 2025
  )

  expect_equal(resultado$TIPO_JORNADA, c("Integral", "Parcial"))
  expect_equal(resultado$TOTAL_MATRICULAS, c(40, 60))
  expect_equal(sum(resultado$TOTAL_MATRICULAS), 100)
})

test_that("resumo seleciona e renomeia as colunas da projecao", {
  matriculas <- data.frame(
    ANO = 2026,
    UF = 12,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    grupo_localizacao = "Capital",
    ETAPA_ENSINO = "EM",
    ETAPA_ENSINO_DETALHE = "EM_PROP",
    P_COMPOSICAO = 0.8,
    TOTAL_MATRICULAS = 40,
    TIPO_JORNADA = "Integral",
    P_JORNADA = 0.4,
    COLUNA_EXTRA = "remover"
  )
  atu <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = "Capital",
    ETAPA_ENSINO = "EM",
    MEDIA_ALUNOS_TURMA = 27.5
  )

  resultado <- resumir_matriculas_projetadas(
    matriculas_projetadas = matriculas,
    media_alunos_turma_localizacao = atu,
    ano_referencia_atu = 2025
  )

  expect_named(
    resultado,
    c(
      "ANO",
      "UF",
      "SIGLA_UF",
      "NO_UF",
      "grupo_localizacao",
      "ETAPA_ENSINO",
      "ETAPA_ENSINO_DETALHE",
      "P_ETAPA_DETALHE",
      "TIPO_JORNADA",
      "P_JORNADA",
      "QT_MATRICULA",
      "MEDIA_ALUNOS_TURMA"
    )
  )
  expect_equal(resultado$P_ETAPA_DETALHE, 0.8)
  expect_equal(resultado$QT_MATRICULA, 40)
  expect_equal(resultado$MEDIA_ALUNOS_TURMA, 27.5)
  expect_false("COLUNA_EXTRA" %in% names(resultado))
})

test_that("resumo exige ATU quando ha matriculas projetadas", {
  matriculas <- data.frame(
    ANO = 2026,
    UF = 12,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    grupo_localizacao = "Capital",
    ETAPA_ENSINO = "CRE",
    ETAPA_ENSINO_DETALHE = "CRE",
    P_COMPOSICAO = 1,
    TOTAL_MATRICULAS = 10,
    TIPO_JORNADA = "Parcial",
    P_JORNADA = 1
  )
  atu <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = "Capital",
    ETAPA_ENSINO = "CRE",
    MEDIA_ALUNOS_TURMA = NA_real_
  )

  expect_error(
    resumir_matriculas_projetadas(
      matriculas_projetadas = matriculas,
      media_alunos_turma_localizacao = atu
    ),
    "matriculas projetadas positivas sem media"
  )
})
