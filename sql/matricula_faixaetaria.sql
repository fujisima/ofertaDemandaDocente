WITH
matriculas_creche AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM (
    SELECT
      *,
      'CRE' AS ETAPA_ENSINO
    FROM fujisima_db.marts.tb_educacao_basica_uf_creche_faixa_etaria
  )
  UNPIVOT (
    QT_MAT FOR FAIXA_ETARIA IN (
      QT_MAT_INF_CRE,
      QT_MAT_INF_CRE_03,
      QT_MAT_INF_CRE_45,
      QT_MAT_INF_CRE_6_MAIS
    )
  )
),

matriculas_preescola AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM (
    SELECT
      *,
      'PRE' AS ETAPA_ENSINO
    FROM fujisima_db.marts.tb_educacao_basica_uf_preescola_faixa_etaria
  )
  UNPIVOT (
    QT_MAT FOR FAIXA_ETARIA IN (
      QT_MAT_INF_PRE,
      QT_MAT_INF_PRE_03,
      QT_MAT_INF_PRE_45,
      QT_MAT_INF_PRE_6_MAIS
    )
  )
),

matriculas_anos_iniciais AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM (
    SELECT
      *,
      'AI' AS ETAPA_ENSINO
    FROM fujisima_db.marts.tb_educacao_basica_uf_anosiniciais_faixa_etaria
  )
  UNPIVOT (
    QT_MAT FOR FAIXA_ETARIA IN (
      QT_MAT_FUND_AI,
      QT_MAT_FUND_AI_05,
      QT_MAT_FUND_AI_610,
      QT_MAT_FUND_AI_1114,
      QT_MAT_FUND_AI_1517,
      QT_MAT_FUND_AI_1819,
      QT_MAT_FUND_AI_20_MAIS
    )
  )
),

matriculas_anos_finais AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM (
    SELECT
      *,
      'AF' AS ETAPA_ENSINO
    FROM fujisima_db.marts.tb_educacao_basica_uf_anosfinais_faixa_etaria
  )
  UNPIVOT (
    QT_MAT FOR FAIXA_ETARIA IN (
      QT_MAT_FUND_AF,
      QT_MAT_FUND_AF_010,
      QT_MAT_FUND_AF_1114,
      QT_MAT_FUND_AF_1517,
      QT_MAT_FUND_AF_1819,
      QT_MAT_FUND_AF_2024,
      QT_MAT_FUND_AF_25_MAIS
    )
  )
),

matriculas_ensino_medio AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM (
    SELECT
      *,
      'EM' AS ETAPA_ENSINO
    FROM fujisima_db.marts.tb_educacao_basica_uf_medio_faixa_etaria
  )
  UNPIVOT (
    QT_MAT FOR FAIXA_ETARIA IN (
      QT_MAT_MED,
      QT_MAT_MED_014,
      QT_MAT_MED_1517,
      QT_MAT_MED_1819,
      QT_MAT_MED_2024,
      QT_MAT_MED_25_MAIS
    )
  )
),

matriculas_unificadas AS (
  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM matriculas_creche

  UNION ALL

  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM matriculas_preescola

  UNION ALL

  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM matriculas_anos_iniciais

  UNION ALL

  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM matriculas_anos_finais

  UNION ALL

  SELECT
    ANO,
    NO_REGIAO_GEOGRAFICA,
    NO_UF,
    ETAPA_ENSINO,
    FAIXA_ETARIA,
    QT_MAT
  FROM matriculas_ensino_medio
)

SELECT
  ANO,
  NO_REGIAO_GEOGRAFICA,
  NO_UF,
  ETAPA_ENSINO,
  CASE ETAPA_ENSINO
    WHEN 'CRE' THEN 'Creche'
    WHEN 'PRE' THEN 'Pre-escola'
    WHEN 'AI' THEN 'Anos iniciais'
    WHEN 'AF' THEN 'Anos finais'
    WHEN 'EM' THEN 'Ensino medio'
  END AS ETAPA_ENSINO_NOME,
  FAIXA_ETARIA AS FAIXA_ETARIA_ORIGINAL,
  CASE
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE',
      'QT_MAT_INF_PRE',
      'QT_MAT_FUND_AI',
      'QT_MAT_FUND_AF',
      'QT_MAT_MED'
    ) THEN 'Total'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE_03',
      'QT_MAT_INF_PRE_03'
    ) THEN '0 a 3 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE_45',
      'QT_MAT_INF_PRE_45'
    ) THEN '4 a 5 anos'
    WHEN FAIXA_ETARIA = 'QT_MAT_INF_CRE_6_MAIS' THEN '6 anos ou mais'
    WHEN FAIXA_ETARIA = 'QT_MAT_INF_PRE_6_MAIS' THEN '6 anos ou mais'
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_05' THEN '0 a 5 anos'
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_610' THEN '6 a 10 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1114',
      'QT_MAT_FUND_AF_1114'
    ) THEN '11 a 14 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1517',
      'QT_MAT_FUND_AF_1517',
      'QT_MAT_MED_1517'
    ) THEN '15 a 17 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1819',
      'QT_MAT_FUND_AF_1819',
      'QT_MAT_MED_1819'
    ) THEN '18 a 19 anos'
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_20_MAIS' THEN '20 anos ou mais'
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AF_010' THEN '0 a 10 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AF_2024',
      'QT_MAT_MED_2024'
    ) THEN '20 a 24 anos'
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AF_25_MAIS',
      'QT_MAT_MED_25_MAIS'
    ) THEN '25 anos ou mais'
    WHEN FAIXA_ETARIA = 'QT_MAT_MED_014' THEN '0 a 14 anos'
  END AS FAIXA_ETARIA,
  CASE
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE',
      'QT_MAT_INF_PRE',
      'QT_MAT_FUND_AI',
      'QT_MAT_FUND_AF',
      'QT_MAT_MED'
    ) THEN 0
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE_03',
      'QT_MAT_INF_PRE_03'
    ) THEN 1
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_INF_CRE_45',
      'QT_MAT_INF_PRE_45'
    ) THEN 2
    WHEN FAIXA_ETARIA = 'QT_MAT_INF_CRE_6_MAIS' THEN 3
    WHEN FAIXA_ETARIA = 'QT_MAT_INF_PRE_6_MAIS' THEN 3
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_05' THEN 1
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_610' THEN 2
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1114',
      'QT_MAT_FUND_AF_1114'
    ) THEN 3
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1517',
      'QT_MAT_FUND_AF_1517',
      'QT_MAT_MED_1517'
    ) THEN 4
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AI_1819',
      'QT_MAT_FUND_AF_1819',
      'QT_MAT_MED_1819'
    ) THEN 5
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AI_20_MAIS' THEN 6
    WHEN FAIXA_ETARIA = 'QT_MAT_FUND_AF_010' THEN 1
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AF_2024',
      'QT_MAT_MED_2024'
    ) THEN 6
    WHEN FAIXA_ETARIA IN (
      'QT_MAT_FUND_AF_25_MAIS',
      'QT_MAT_MED_25_MAIS'
    ) THEN 7
    WHEN FAIXA_ETARIA = 'QT_MAT_MED_014' THEN 1
  END AS ORDEM_FAIXA_ETARIA,
  QT_MAT
FROM matriculas_unificadas
ORDER BY
  ANO,
  NO_UF,
  ETAPA_ENSINO,
  ORDEM_FAIXA_ETARIA
;
