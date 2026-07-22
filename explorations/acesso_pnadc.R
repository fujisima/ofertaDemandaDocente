# Obtém dados da Pnad Contínua 
acesso_pnadc <- targets::tar_read(acesso_pnadc)

library(dplyr)

#==================================================================
#= Análise exploratória dos dados de acesso - Creche (0 a 3 anos) = 
#==================================================================

acesso_pnadc %>% 
  filter(ANO==2025 & INDICADOR=="1B") %>% 
  select(NO_UF, INDICADOR, PERCENTUAL, COEF_VARIACAO)

# Meta do PNE: 60% até 2036

#======================================================================
#= Análise exploratória dos dados de acesso - Pré-Escola (4 a 5 anos) = 
#======================================================================

acesso_pnadc %>% 
  filter(ANO==2025 & INDICADOR=="1A") %>% 
  select(NO_UF, INDICADOR, PERCENTUAL, COEF_VARIACAO)

#===============================================================================
#= Análise exploratória dos dados de acesso - Ensino Fundamental (6 a 14 anos) = 
#===============================================================================

acesso_pnadc %>% 
  filter(ANO==2025 & INDICADOR=="2A") %>% 
  select(NO_UF, INDICADOR, PERCENTUAL, COEF_VARIACAO)

#==========================================================================
#= Análise exploratória dos dados de acesso - Ensino Médio (15 a 17 anos) = 
#==========================================================================

acesso_pnadc %>% 
  filter(ANO==2025 & INDICADOR=="3B") %>% 
  select(NO_UF, INDICADOR, PERCENTUAL, COEF_VARIACAO)


