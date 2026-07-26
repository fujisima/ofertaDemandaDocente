WITH
matriculas AS (
  SELECT
    *
  FROM fujisima_db.marts.tb_educacao_basica_uf_matricula_faixa_etaria
),

matriculas_longas AS (
  UNPIVOT matriculas
  ON COLUMNS('^QT_MAT')
  INTO
    NAME FAIXA_ETARIA_ORIGINAL
    VALUE QT_MAT
)

SELECT
  ANO,
  NO_REGIAO_GEOGRAFICA,
  NO_UF,
  CAST(NULL AS VARCHAR) AS ETAPA_ENSINO,
  CAST(NULL AS VARCHAR) AS ETAPA_ENSINO_NOME,
  FAIXA_ETARIA_ORIGINAL,
  CASE
    WHEN FAIXA_ETARIA_ORIGINAL IN (
      'QT_MAT',
      'QT_MAT_TOTAL',
      'QT_MAT_BAS'
    ) THEN 'Total'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_0_3|_03)$') THEN '0 a 3 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_4_5|_45)$') THEN '4 a 5 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_6_10|_610)$') THEN '6 a 10 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_11_14|_1114)$') THEN '11 a 14 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_15_17|_1517)$') THEN '15 a 17 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_18_19|_1819)$') THEN '18 a 19 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_20_24|_2024)$') THEN '20 a 24 anos'
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '_25_MAIS$') THEN '25 anos ou mais'
  END AS FAIXA_ETARIA,
  CASE
    WHEN FAIXA_ETARIA_ORIGINAL IN (
      'QT_MAT',
      'QT_MAT_TOTAL',
      'QT_MAT_BAS'
    ) THEN 0
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_0_3|_03)$') THEN 1
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_4_5|_45)$') THEN 2
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_6_10|_610)$') THEN 3
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_11_14|_1114)$') THEN 4
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_15_17|_1517)$') THEN 5
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_18_19|_1819)$') THEN 6
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '(_20_24|_2024)$') THEN 7
    WHEN regexp_matches(FAIXA_ETARIA_ORIGINAL, '_25_MAIS$') THEN 8
  END AS ORDEM_FAIXA_ETARIA,
  QT_MAT
FROM matriculas_longas
ORDER BY
  ANO,
  NO_UF,
  ORDEM_FAIXA_ETARIA
;
