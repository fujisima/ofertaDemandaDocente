SELECT
  ANO,
  NIVEL,
  NO_UF,
  INDICADOR,
  ESTIMATIVA,
  COEF_VARIACAO,
  PERCENTUAL
FROM fujisima_db.view.tb_educacao_basica_acesso
WHERE
  NIVEL = 'uf'
  AND FREQUENTA = 'sim'
;
