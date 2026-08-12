library(targets)

source("R/motherduck.R")
source("R/dados.R")
source("R/indicadores_matricula.R")
source("R/indicadores_observados.R")
source("R/metas_indicadores.R")
source("R/projecao_logistica.R")
source("R/projecao_indicadores_matricula.R")

options(ofertaDemandaDocente.sql_dir = "sql")

tar_option_set(
  packages = c("DBI", "duckdb")
)

list(
  tar_target(
    matricula_faixaetaria,
    ler_matricula_faixaetaria()
  ),
  tar_target(
    matricula_faixaetaria_etapa,
    ler_matricula_faixaetaria_etapa()
  ),
  tar_target(
    projecao_populacional_ibge,
    ler_projecao_populacional_ibge()
  ),
  tar_target(
    indicadores_matricula_observados,
    calcular_indicadores_matricula_observados(
      matriculas = matricula_faixaetaria_etapa,
      populacao = projecao_populacional_ibge,
      matriculas_faixaetaria = matricula_faixaetaria
    )
  ),
  tar_target(
    metas_indicadores,
    criar_metas_indicadores_gerais(
      taxa_bruta_matricula = c(
        "0 a 3 anos" = 60,
        "4 a 5 anos" = 100,
        "6 a 10 anos" = 100,
        "11 a 14 anos" = 100,
        "15 a 17 anos" = 100,
        "18 a 19 anos" = 10
      ),
      taxa_liquida_matricula = c(
        "0 a 3 anos" = 60,
        "4 a 5 anos" = 100,
        "6 a 10 anos" = 100,
        "11 a 14 anos" = 95,
        "15 a 17 anos" = 90
      ),
      ano_target = 2036
    )
  ),
  tar_target(
    indicadores_matricula_projetados,
    projetar_indicadores_matricula(
      indicadores_matricula_observados,
      projecao_populacional_ibge,
      metas_indicadores,
      anos = 2026:2036,
      ano_base = 2025
    )
  ),
  tar_target(
    matriculas_etapa_projetadas,
    compor_matriculas_etapa_projetadas(indicadores_matricula_projetados)
  )
)
