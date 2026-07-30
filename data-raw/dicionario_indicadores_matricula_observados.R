dicionario_fonte_sinopse <- "Sinopse Estatistica da Educacao Basica"
dicionario_fonte_ibge <- "IBGE"
dicionario_fonte_pacote <- "Calculado pelo pacote"

dicionario_tabela_etapas <- paste(
  "Tabelas de matriculas por faixa etaria das etapas CRE, PRE, AI, AF e EM",
  "da Sinopse Estatistica da Educacao Basica"
)
dicionario_tabela_faixa_etaria <- paste(
  "Numero de Matriculas da Educacao Basica, por Faixa Etaria, segundo a",
  "Regiao Geografica, a Unidade da Federacao e o Municipio"
)
dicionario_tabela_populacao <- "Projecao populacional do IBGE por UF, ano e faixa etaria adequada"
dicionario_tabela_calculo <- "Derivado de variaveis calculadas pelo pacote"

dicionario_indicadores_matricula_observados <- data.frame(
  variavel = c(
    "ANO",
    "SIGLA_UF",
    "NO_UF",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "TOTAL_MATRICULAS",
    "MAT_FAIXA_ADEQUADA",
    "MAT_FORA_FAIXA",
    "MAT_FAIXA_ETARIA",
    "MAT_TAXA_BRUTA_MATRICULA",
    "POP_FAIXA_ADEQUADA",
    "TAXA_LIQUIDA_MATRICULA",
    "TAXA_BRUTA_MATRICULA",
    "PERCENTUAL_MATRICULAS_FORA_FAIXA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II",
    "TAXA_AVANCADOS",
    "TAXA_DEFASAGEM_I",
    "TAXA_DEFASAGEM_II",
    "TIPO_CALCULO_AVANCADOS",
    "TIPO_CALCULO_DEFASAGEM_I",
    "TIPO_CALCULO_DEFASAGEM_II"
  ),
  descricao = c(
    "Ano de referencia do indicador.",
    "Sigla da unidade da federacao.",
    "Nome da unidade da federacao.",
    "Codigo da etapa de ensino.",
    "Nome da etapa de ensino.",
    "Total de matriculas na etapa de ensino.",
    "Matriculas na faixa etaria adequada da etapa de ensino.",
    "Matriculas fora da faixa etaria adequada da etapa de ensino.",
    "Total amplo de matriculas na faixa etaria adequada, sem recorte por etapa.",
    "Numerador usado para calcular a taxa bruta de matricula.",
    "Populacao da faixa etaria adequada da etapa de ensino.",
    "Taxa liquida de matricula.",
    "Taxa bruta de matricula no universo das etapas modeladas.",
    "Percentual de matriculas fora da faixa etaria adequada.",
    "Matriculas em etapas mais avancadas que a etapa adequada.",
    "Matriculas em defasagem I em relacao a etapa adequada.",
    "Matriculas em defasagem II em relacao a etapa adequada.",
    "Taxa de matriculas em etapas mais avancadas.",
    "Taxa de matriculas em defasagem I.",
    "Taxa de matriculas em defasagem II.",
    "Tipo de calculo usado para o numerador de avancados.",
    "Tipo de calculo usado para o numerador de defasagem I.",
    "Tipo de calculo usado para o numerador de defasagem II."
  ),
  unidade = c(
    "ano",
    "texto",
    "texto",
    "texto",
    "texto",
    "matriculas",
    "matriculas",
    "matriculas",
    "matriculas",
    "matriculas",
    "pessoas",
    "percentual",
    "percentual",
    "percentual",
    "matriculas",
    "matriculas",
    "matriculas",
    "percentual",
    "percentual",
    "percentual",
    "texto",
    "texto",
    "texto"
  ),
  fonte_dados = c(
    rep("Chaves de identificacao das bases de origem", 5),
    rep(dicionario_fonte_sinopse, 5),
    dicionario_fonte_ibge,
    rep(dicionario_fonte_pacote, 3),
    rep(dicionario_fonte_sinopse, 3),
    rep(dicionario_fonte_pacote, 6)
  ),
  tabela_origem = c(
    rep("Bases de matriculas por etapa e populacao por faixa etaria adequada", 5),
    dicionario_tabela_etapas,
    dicionario_tabela_etapas,
    dicionario_tabela_calculo,
    dicionario_tabela_faixa_etaria,
    dicionario_tabela_calculo,
    dicionario_tabela_populacao,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_etapas,
    dicionario_tabela_etapas,
    dicionario_tabela_etapas,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo,
    dicionario_tabela_calculo
  ),
  observacao = c(
    "",
    "",
    "",
    "Valores esperados: CRE, PRE, AI, AF ou EM.",
    "",
    "Calculado a partir das matriculas por faixa etaria e etapa, usando a linha Total.",
    "Usado como numerador da taxa liquida de matricula.",
    "Diferenca entre TOTAL_MATRICULAS e MAT_FAIXA_ADEQUADA.",
    "Quando informado, vem da tabela geral de matriculas por faixa etaria e pode incluir modalidades tratadas separadamente.",
    "Soma de MAT_FAIXA_ADEQUADA, MAT_AVANCADOS, MAT_DEFASAGEM_I e MAT_DEFASAGEM_II; valores ausentes sao tratados como zero.",
    "Denominador das taxas liquida, bruta, avancados e defasagens.",
    "MAT_FAIXA_ADEQUADA / POP_FAIXA_ADEQUADA * 100.",
    "MAT_TAXA_BRUTA_MATRICULA / POP_FAIXA_ADEQUADA * 100.",
    "MAT_FORA_FAIXA / TOTAL_MATRICULAS * 100.",
    "Pode ser nao aplicavel em algumas etapas.",
    "Pode ser exato ou aproximado, conforme TIPO_CALCULO_DEFASAGEM_I.",
    "Pode ser exato, aproximado ou nao aplicavel, conforme TIPO_CALCULO_DEFASAGEM_II.",
    "MAT_AVANCADOS / POP_FAIXA_ADEQUADA * 100; ausente quando nao aplicavel.",
    "MAT_DEFASAGEM_I / POP_FAIXA_ADEQUADA * 100; ausente quando nao aplicavel.",
    "MAT_DEFASAGEM_II / POP_FAIXA_ADEQUADA * 100; ausente quando nao aplicavel.",
    "Valores esperados: exato, aproximado ou nao_aplicavel.",
    "Valores esperados: exato, aproximado ou nao_aplicavel.",
    "Valores esperados: exato, aproximado ou nao_aplicavel."
  ),
  stringsAsFactors = FALSE
)

save(
  dicionario_indicadores_matricula_observados,
  file = "data/dicionario_indicadores_matricula_observados.rda",
  compress = "xz"
)
