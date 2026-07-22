SELECT
  ANO,
  COUNT(*) AS QTD
FROM fujisima_db.view.tb_educacao_basica_acesso
GROUP BY ANO
ORDER BY 1;
