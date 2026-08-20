test_that("rateio por localizacao preserva os totais estaduais", {
  indicadores <- data.frame(
    ANO = 2025,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    FAIXA_ETARIA = "6 a 10 anos",
    ETAPA_ENSINO_ADEQUADA = "AI",
    ETAPA_ENSINO_ADEQUADA_NOME = "Anos iniciais",
    MAT_FAIXA_ADEQUADA = 100,
    MAT_AVANCADOS = 20,
    MAT_DEFASAGEM_I = 10,
    MAT_DEFASAGEM_II = NA_real_
  )
  populacao <- data.frame(
    ano_referencia = 2025,
    trimestre_referencia = 2,
    UF = 12,
    faixa_etaria = "6 a 10 anos",
    criterio_localizacao = "Capital",
    grupo_localizacao = c("Polo urbano", "Demais áreas"),
    populacao_estimada = c(400, 600),
    erro_padrao = c(20, 30),
    coeficiente_variacao = c(0.05, 0.05),
    percentual_uf_faixa = c(40, 60)
  )

  resultado <- ratear_matriculas_por_localizacao(
    indicadores = indicadores,
    populacao_localizacao = populacao,
    ano = 2025
  )

  expect_equal(nrow(resultado), 2)
  expect_equal(resultado$SIGLA_UF, c("AC", "AC"))
  expect_equal(resultado$grupo_localizacao, c("Polo urbano", "Demais áreas"))
  expect_equal(resultado$MAT_FAIXA_ADEQUADA, c(40, 60))
  expect_equal(resultado$MAT_AVANCADOS, c(8, 12))
  expect_equal(resultado$MAT_DEFASAGEM_I, c(4, 6))
  expect_true(all(is.na(resultado$MAT_DEFASAGEM_II)))
})

test_that("rateio das projecoes aplica os percentuais de 2025 a todos os anos", {
  indicadores <- data.frame(
    ANO = c(2026, 2036),
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    FAIXA_ETARIA = "6 a 10 anos",
    MAT_FAIXA_ADEQUADA = c(100, 200),
    MAT_AVANCADOS = c(20, 40),
    MAT_DEFASAGEM_I = c(10, 20),
    MAT_DEFASAGEM_II = c(5, 10)
  )
  populacao <- data.frame(
    ano_referencia = 2025,
    UF = 12,
    faixa_etaria = "6 a 10 anos",
    criterio_localizacao = "Capital",
    grupo_localizacao = c("Polo urbano", "Demais áreas"),
    percentual_uf_faixa = c(40, 60)
  )

  resultado <- ratear_matriculas_projetadas_por_localizacao(
    indicadores_projetados = indicadores,
    populacao_localizacao = populacao,
    ano_base_localizacao = 2025
  )

  expect_equal(nrow(resultado), 4)
  expect_equal(resultado$MAT_FAIXA_ADEQUADA, c(40, 60, 80, 120))
  expect_equal(resultado$MAT_AVANCADOS, c(8, 12, 16, 24))
  expect_equal(resultado$MAT_DEFASAGEM_I, c(4, 6, 8, 12))
  expect_equal(resultado$MAT_DEFASAGEM_II, c(2, 3, 4, 6))
})

test_that("composicao por etapa preserva totais entre grupos de localizacao", {
  indicadores_uf <- data.frame(
    ANO = 2036,
    SIGLA_UF = "AC",
    NO_UF = "Acre",
    FAIXA_ETARIA = c(
      "0 a 3 anos", "4 a 5 anos", "6 a 10 anos",
      "11 a 14 anos", "15 a 17 anos", "18 a 19 anos"
    ),
    MAT_FAIXA_ADEQUADA = c(70, 80, 90, 100, 110, NA),
    MAT_AVANCADOS = c(5, 6, 7, 8, NA, NA),
    MAT_DEFASAGEM_I = c(NA, 9, 10, 11, 12, 13),
    MAT_DEFASAGEM_II = c(NA, NA, 14, NA, 15, 16)
  )
  polo <- indicadores_uf
  demais <- indicadores_uf
  colunas_matriculas <- c(
    "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I", "MAT_DEFASAGEM_II"
  )
  polo[colunas_matriculas] <- polo[colunas_matriculas] * 0.4
  demais[colunas_matriculas] <- demais[colunas_matriculas] * 0.6
  polo$UF <- 12
  demais$UF <- 12
  polo$criterio_localizacao <- "Capital"
  demais$criterio_localizacao <- "Capital"
  polo$grupo_localizacao <- "Polo urbano"
  demais$grupo_localizacao <- "Demais áreas"
  percentuais_em <- data.frame(
    ANO = 2025,
    NO_UF = "Acre",
    GRUPO_LOCALIZACAO = c("Capital", "Interior"),
    P_EM_PROPEDEUTICO = c(0.8, 0.7),
    P_EM_EPT = c(0.2, 0.3)
  )

  resultado_uf <- compor_matriculas_etapa_projetadas(indicadores_uf)
  resultado_localizacao <- compor_matriculas_etapa_projetadas_localizacao(
    indicadores_projetados_localizacao = rbind(polo, demais),
    percentuais_em_localizacao = percentuais_em,
    ano_referencia_composicao = 2025
  )
  totais_localizacao <- aggregate(
    TOTAL_MATRICULAS ~ ANO + SIGLA_UF + NO_UF + ETAPA_ENSINO,
    data = resultado_localizacao,
    FUN = sum
  )
  totais_localizacao <- totais_localizacao[
    order(totais_localizacao$ETAPA_ENSINO),
  ]
  resultado_uf <- resultado_uf[order(resultado_uf$ETAPA_ENSINO), ]

  expect_equal(
    totais_localizacao$TOTAL_MATRICULAS,
    resultado_uf$TOTAL_MATRICULAS
  )
  expect_equal(
    unique(resultado_localizacao$grupo_localizacao),
    c("Capital", "Interior")
  )
  expect_equal(
    unique(
      resultado_localizacao$ETAPA_ENSINO_DETALHE[
        resultado_localizacao$ETAPA_ENSINO == "EM"
      ]
    ),
    c("EM_PROP", "EM_EPT")
  )
  expect_equal(
    resultado_localizacao$P_COMPOSICAO[
      resultado_localizacao$ETAPA_ENSINO == "EM"
    ],
    c(0.8, 0.7, 0.2, 0.3)
  )
})

test_that("composicao ausente aceita total zero e rejeita total positivo", {
  matriculas <- data.frame(
    ANO = 2030,
    UF = 53,
    SIGLA_UF = "DF",
    NO_UF = "Distrito Federal",
    criterio_localizacao = "Capital",
    grupo_localizacao = "Interior",
    ETAPA_ENSINO = "EM",
    ETAPA_ENSINO_NOME = "Ensino medio",
    TOTAL_MATRICULAS = 0
  )
  percentuais <- data.frame(
    ANO = 2025,
    NO_UF = "Distrito Federal",
    GRUPO_LOCALIZACAO = "Interior",
    P_EM_PROPEDEUTICO = NA_real_,
    P_EM_EPT = NA_real_
  )

  resultado <- detalhar_ensino_medio_localizacao(
    matriculas_etapa = matriculas,
    percentuais_em_localizacao = percentuais,
    ano_referencia_composicao = 2025
  )

  expect_equal(resultado$ETAPA_ENSINO_DETALHE, c("EM_PROP", "EM_EPT"))
  expect_equal(resultado$TOTAL_MATRICULAS, c(0, 0))
  expect_true(all(is.na(resultado$P_COMPOSICAO)))

  matriculas$TOTAL_MATRICULAS <- 100
  expect_error(
    detalhar_ensino_medio_localizacao(
      matriculas_etapa = matriculas,
      percentuais_em_localizacao = percentuais,
      ano_referencia_composicao = 2025
    ),
    "sem percentual de composicao"
  )
})
