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
    projecao_populacional_ibge,
    ler_projecao_populacional_ibge()
  ),
  tar_target(
    indicadores_matricula_observados,
    calcular_indicadores_matricula_observados(
      matricula_faixaetaria,
      projecao_populacional_ibge
    )
  ),
  tar_target(
    metas_indicadores,
    criar_metas_indicadores_gerais(
      taxa_liquida_matricula = c(
        CRE = 70,
        PRE = 99,
        AI  = 99,
        AF  = 99,
        EM  = 99
      ),
      percentual_matriculas_fora_faixa = c(
        CRE = 1,
        PRE = 1,
        AI  = 5,
        AF  = 5,
        EM  = 5
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
  )
)
