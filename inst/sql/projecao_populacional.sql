WITH populacao_por_etapa AS (
  SELECT
    SIGLA,
    LOCAL,
    SEXO,
    CASE
      WHEN IDADE BETWEEN 0 AND 3 THEN 'CRE'
      WHEN IDADE BETWEEN 4 AND 5 THEN 'PRE'
      WHEN IDADE BETWEEN 6 AND 10 THEN 'AI'
      WHEN IDADE BETWEEN 11 AND 14 THEN 'AF'
      WHEN IDADE BETWEEN 15 AND 17 THEN 'EM'
      WHEN IDADE BETWEEN 18 AND 19 THEN 'POS_EM'
      ELSE 'OUTROS'
    END AS ETAPA_ENSINO,
    SUM("2000") AS "2000",
    SUM("2001") AS "2001",
    SUM("2002") AS "2002",
    SUM("2003") AS "2003",
    SUM("2004") AS "2004",
    SUM("2005") AS "2005",
    SUM("2006") AS "2006",
    SUM("2007") AS "2007",
    SUM("2008") AS "2008",
    SUM("2009") AS "2009",
    SUM("2010") AS "2010",
    SUM("2011") AS "2011",
    SUM("2012") AS "2012",
    SUM("2013") AS "2013",
    SUM("2014") AS "2014",
    SUM("2015") AS "2015",
    SUM("2016") AS "2016",
    SUM("2017") AS "2017",
    SUM("2018") AS "2018",
    SUM("2019") AS "2019",
    SUM("2020") AS "2020",
    SUM("2021") AS "2021",
    SUM("2022") AS "2022",
    SUM("2023") AS "2023",
    SUM("2024") AS "2024",
    SUM("2025") AS "2025",
    SUM("2026") AS "2026",
    SUM("2027") AS "2027",
    SUM("2028") AS "2028",
    SUM("2029") AS "2029",
    SUM("2030") AS "2030",
    SUM("2031") AS "2031",
    SUM("2032") AS "2032",
    SUM("2033") AS "2033",
    SUM("2034") AS "2034",
    SUM("2035") AS "2035",
    SUM("2036") AS "2036",
    SUM("2037") AS "2037",
    SUM("2038") AS "2038",
    SUM("2039") AS "2039",
    SUM("2040") AS "2040",
    SUM("2041") AS "2041",
    SUM("2042") AS "2042",
    SUM("2043") AS "2043",
    SUM("2044") AS "2044",
    SUM("2045") AS "2045",
    SUM("2046") AS "2046",
    SUM("2047") AS "2047",
    SUM("2048") AS "2048",
    SUM("2049") AS "2049",
    SUM("2050") AS "2050",
    SUM("2051") AS "2051",
    SUM("2052") AS "2052",
    SUM("2053") AS "2053",
    SUM("2054") AS "2054",
    SUM("2055") AS "2055",
    SUM("2056") AS "2056",
    SUM("2057") AS "2057",
    SUM("2058") AS "2058",
    SUM("2059") AS "2059",
    SUM("2060") AS "2060",
    SUM("2061") AS "2061",
    SUM("2062") AS "2062",
    SUM("2063") AS "2063",
    SUM("2064") AS "2064",
    SUM("2065") AS "2065",
    SUM("2066") AS "2066",
    SUM("2067") AS "2067",
    SUM("2068") AS "2068",
    SUM("2069") AS "2069",
    SUM("2070") AS "2070"
  FROM fujisima_db.marts.tb_projecao_populacional_ibge
  WHERE
    SEXO = 'Ambos'
    AND LOCAL NOT IN (
      'Brasil',
      'Norte',
      'Nordeste',
      'Sudeste',
      'Sul',
      'Centro-Oeste'
    )
  GROUP BY
    SIGLA,
    LOCAL,
    SEXO,
    ETAPA_ENSINO
),

populacao_long AS (
  SELECT
    SIGLA,
    LOCAL,
    SEXO,
    ETAPA_ENSINO,
    CAST(ANO AS INTEGER) AS ANO,
    POPULACAO
  FROM populacao_por_etapa
  UNPIVOT (
    POPULACAO FOR ANO IN (
      "2000",
      "2001",
      "2002",
      "2003",
      "2004",
      "2005",
      "2006",
      "2007",
      "2008",
      "2009",
      "2010",
      "2011",
      "2012",
      "2013",
      "2014",
      "2015",
      "2016",
      "2017",
      "2018",
      "2019",
      "2020",
      "2021",
      "2022",
      "2023",
      "2024",
      "2025",
      "2026",
      "2027",
      "2028",
      "2029",
      "2030",
      "2031",
      "2032",
      "2033",
      "2034",
      "2035",
      "2036",
      "2037",
      "2038",
      "2039",
      "2040",
      "2041",
      "2042",
      "2043",
      "2044",
      "2045",
      "2046",
      "2047",
      "2048",
      "2049",
      "2050",
      "2051",
      "2052",
      "2053",
      "2054",
      "2055",
      "2056",
      "2057",
      "2058",
      "2059",
      "2060",
      "2061",
      "2062",
      "2063",
      "2064",
      "2065",
      "2066",
      "2067",
      "2068",
      "2069",
      "2070"
    )
  )
  WHERE ETAPA_ENSINO <> 'OUTROS'
)

SELECT
  SIGLA,
  LOCAL,
  SEXO,
  ETAPA_ENSINO,
  CASE ETAPA_ENSINO
    WHEN 'CRE' THEN 'Creche'
    WHEN 'PRE' THEN 'Pre-escola'
    WHEN 'AI' THEN 'Anos iniciais'
    WHEN 'AF' THEN 'Anos finais'
    WHEN 'EM' THEN 'Ensino medio'
    WHEN 'POS_EM' THEN 'Pos-ensino medio'
  END AS ETAPA_ENSINO_NOME,
  ANO,
  POPULACAO
FROM populacao_long
ORDER BY
  SIGLA,
  ANO,
  ETAPA_ENSINO
;
