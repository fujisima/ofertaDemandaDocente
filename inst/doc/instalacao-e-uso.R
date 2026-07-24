## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-package-------------------------------------------------------------
library(ofertaDemandaDocente)

## ----taxa-liquida-------------------------------------------------------------
taxa_liquida_matricula(
  matriculas_faixa_adequada = 850,
  populacao_faixa_adequada = 1000
)

## ----fora-faixa---------------------------------------------------------------
percentual_matriculas_fora_faixa(
  matriculas_fora_faixa = 120,
  total_matriculas = 1000
)

## ----projecao-logistica-------------------------------------------------------
anos <- 2026:2036

projecao_logistica_target(
  t0 = 2025,
  tf = 2036,
  lim = 100,
  obs0 = 78,
  target = 95,
  t = anos
)

## ----metas--------------------------------------------------------------------
metas <- criar_metas_indicadores_gerais(
  taxa_liquida_matricula = c(
    CRE = 60,
    PRE = 98,
    AI = 99,
    AF = 98,
    EM = 95
  ),
  percentual_matriculas_fora_faixa = c(
    CRE = 5,
    PRE = 3,
    AI = 2,
    AF = 4,
    EM = 6
  ),
  ano_target = 2036
)

metas
