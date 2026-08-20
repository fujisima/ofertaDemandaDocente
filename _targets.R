library(targets)

source("R/motherduck.R")
source("R/dados.R")
source("R/composicao_ensino_medio.R")
source("R/alunos_turma.R")
source("R/matriculas_integral.R")
source("R/projecao_matriculas_integral.R")
source("R/indicadores_matricula.R")
source("R/indicadores_observados.R")
source("R/matriculas_localizacao.R")
source("R/metas_indicadores.R")
source("R/projecao_logistica.R")
source("R/projecao_indicadores_matricula.R")
source("R/rascunho_rm.R")

options(ofertaDemandaDocente.sql_dir = "sql")

tar_option_set(
  packages = c(
    "DBI",
    "duckdb",
    "PNADcIBGE",
    "dplyr",
    "readxl",
    "survey",
    "tibble",
    "tidyr"
  )
)

list(
  tar_target(
    composicao_rm_2025_arquivo,
    "data-raw/Composicao_RM_2025_v2.xls",
    format = "file"
  ),
  tar_target(
    municipios_capital_rm_2025,
    ler_municipios_capital_rm(composicao_rm_2025_arquivo)
  ),
  tar_target(
    atu_municipio_2025_arquivo,
    "data-raw/ATU_2025_MUNICIPIOS/ATU_MUNICIPIOS_2025.xlsx",
    format = "file"
  ),
  tar_target(
    atu_municipio_2025,
    ler_atu_municipio_2025(atu_municipio_2025_arquivo)
  ),
  tar_target(
    matriculas_em_municipio_2025,
    ler_matriculas_em_municipio_2025()
  ),
  tar_target(
    percentuais_em_localizacao_2025,
    calcular_percentuais_em_localizacao(
      matriculas_em_municipio = matriculas_em_municipio_2025,
      municipios_capital_rm = municipios_capital_rm_2025
    )
  ),
  tar_target(
    matriculas_integral_em_2025_arquivo,
    "data-raw/Demanda_038281_-_mat_integral_etapa.xlsx",
    format = "file"
  ),
  tar_target(
    matriculas_turno_municipio_2025,
    ler_matriculas_turno_municipio_2025()
  ),
  tar_target(
    media_alunos_turma_localizacao_2025,
    calcular_media_alunos_turma_localizacao(
      atu_municipio = atu_municipio_2025,
      matriculas_turno_municipio = matriculas_turno_municipio_2025,
      matriculas_em_municipio = matriculas_em_municipio_2025,
      municipios_capital_rm = municipios_capital_rm_2025
    )
  ),
  tar_target(
    matriculas_em_integral_2025,
    ler_matriculas_em_integral_2025(
      matriculas_integral_em_2025_arquivo
    )
  ),
  tar_target(
    percentuais_matriculas_integral_localizacao_2025,
    calcular_percentuais_matriculas_integral_localizacao(
      matriculas_turno_municipio = matriculas_turno_municipio_2025,
      matriculas_em_integral = matriculas_em_integral_2025,
      totais_em_localizacao = percentuais_em_localizacao_2025,
      municipios_capital_rm = municipios_capital_rm_2025
    )
  ),
  tar_target(
    metas_percentual_integral,
    criar_metas_percentual_integral(
      percentual_integral = c(
        "CRE" = 50,
        "PRE" = 50,
        "AI" = 50,
        "AF" = 50,
        "EM_PROP" = 50,
        "EM_EPT" = 50
      ),
      incremento_acima_meta = 10,
      ano_target = 2036,
      limite = 100
    )
  ),
  tar_target(
    percentuais_matriculas_integral_projetados,
    projetar_percentuais_matriculas_integral(
      percentuais_observados = percentuais_matriculas_integral_localizacao_2025,
      metas = metas_percentual_integral,
      anos = 2026:2036,
      ano_base = 2025
    )
  ),
  tar_target(
    ano_pnadc_localizacao,
    2025
  ),
  tar_target(
    trimestre_pnadc_localizacao,
    2
  ),
  tar_target(
    pnadc_localizacao,
    ler_pnadc_localizacao(
      ano = ano_pnadc_localizacao,
      trimestre = trimestre_pnadc_localizacao
    )
  ),
  tar_target(
    populacao_localizacao,
    calcular_populacao_localizacao(
      pnadcibge = pnadc_localizacao,
      ano = ano_pnadc_localizacao,
      trimestre = trimestre_pnadc_localizacao
    )
  ),
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
    indicadores_matricula_observados_localizacao,
    ratear_matriculas_por_localizacao(
      indicadores = indicadores_matricula_observados,
      populacao_localizacao = populacao_localizacao,
      ano = ano_pnadc_localizacao
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
    indicadores_matricula_projetados_localizacao,
    ratear_matriculas_projetadas_por_localizacao(
      indicadores_projetados = indicadores_matricula_projetados,
      populacao_localizacao = populacao_localizacao,
      ano_base_localizacao = ano_pnadc_localizacao
    )
  ),
  tar_target(
    matriculas_etapa_projetadas,
    compor_matriculas_etapa_projetadas(indicadores_matricula_projetados)
  ),
  tar_target(
    matriculas_etapa_projetadas_localizacao_base,
    compor_matriculas_etapa_projetadas_localizacao(
      indicadores_projetados_localizacao = indicadores_matricula_projetados_localizacao,
      percentuais_em_localizacao = percentuais_em_localizacao_2025,
      ano_referencia_composicao = 2025
    )
  ),
  tar_target(
    matriculas_etapa_projetadas_localizacao,
    ratear_matriculas_projetadas_por_jornada(
      matriculas_etapa_projetadas_localizacao = matriculas_etapa_projetadas_localizacao_base,
      percentuais_integral_projetados = percentuais_matriculas_integral_projetados,
      ano_referencia = 2025
    )
  ),
  tar_target(
    matriculas_projetadas_resumo,
    resumir_matriculas_projetadas(
      matriculas_projetadas = matriculas_etapa_projetadas_localizacao,
      media_alunos_turma_localizacao = media_alunos_turma_localizacao_2025,
      ano_referencia_atu = 2025
    )
  )
)
