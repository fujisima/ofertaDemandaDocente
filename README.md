# ofertaDemandaDocente

Pacote R para estimar indicadores de acesso escolar e projetar matrículas por
UF, etapa de ensino e ano. O pacote combina dados de matrículas por faixa etária,
projeções populacionais e metas de indicadores para simular trajetórias futuras.

## Instalação

Instale o pacote a partir do repositório remoto com `remotes`:

```r
install.packages("remotes")
remotes::install_github("fujisima/ofertaDemandaDocente")
```

Durante o desenvolvimento local, também é possível instalar a partir da raiz do
projeto:

```r
install.packages("remotes")
remotes::install_local(".")
```

Depois de instalar, carregue o pacote:

```r
library(ofertaDemandaDocente)
```

## Dados incluídos no pacote

As funções `ler_matricula_faixaetaria()`,
`ler_matricula_faixaetaria_etapa()`, `ler_projecao_populacional_ibge()` e
`ler_populacao_municipio_2025()` leem, por padrão, snapshots em Parquet
incluídos no pacote. Assim, depois da instalação, é possível carregar os dados
sem configurar conexão com o MotherDuck:

```r
matriculas <- ler_matricula_faixaetaria()
matriculas_etapa <- ler_matricula_faixaetaria_etapa()
populacao <- ler_projecao_populacional_ibge()
pop_municipio <- ler_populacao_municipio_2025()
```

## Configuração do MotherDuck

O MotherDuck continua disponível como fonte opcional para consultar os dados
diretamente no banco. Para usá-lo, configure um token no arquivo `.Renviron`:

```r
MOTHERDUCK_TOKEN="seu-token"
```

Reinicie a sessão R após editar o `.Renviron`. Para testar se o token está
disponível:

```r
Sys.getenv("MOTHERDUCK_TOKEN")
```

## Uso rápido

### Indicadores simples

```r
taxa_liquida_matricula(
  matriculas_faixa_adequada = 850,
  populacao_faixa_adequada = 1000
)

percentual_matriculas_fora_faixa(
  matriculas_fora_faixa = 120,
  total_matriculas = 1000
)
```

### Projeção logística

```r
projecao_logistica_target(
  t0 = 2025,
  tf = 2036,
  lim = 100,
  obs0 = 78,
  target = 95,
  t = 2026:2036
)
```

### Leitura de dados

Leia os dados incluídos no pacote:

```r
matriculas <- ler_matricula_faixaetaria()
matriculas_etapa <- ler_matricula_faixaetaria_etapa()
populacao <- ler_projecao_populacional_ibge()
pop_municipio <- ler_populacao_municipio_2025()
```

Para consultar diretamente o MotherDuck, use `fonte = "motherduck"`:

```r
matriculas <- ler_matricula_faixaetaria(fonte = "motherduck")
matriculas_etapa <- ler_matricula_faixaetaria_etapa(fonte = "motherduck")
populacao <- ler_projecao_populacional_ibge(fonte = "motherduck")
```

Em seguida, calcule os indicadores observados:

```r
indicadores_observados <- calcular_indicadores_matricula_observados(
  matriculas = matriculas_etapa,
  populacao = populacao,
  matriculas_faixaetaria = matriculas,
  anos = 2016:2025
)
```

Crie metas gerais por faixa etária:

```r
metas <- criar_metas_indicadores_gerais(
  taxa_bruta_matricula = c(
    "0 a 3 anos" = 70,
    "4 a 5 anos" = 99,
    "6 a 10 anos" = 99,
    "11 a 14 anos" = 99,
    "15 a 17 anos" = 99,
    "18 a 19 anos" = 40
  ),
  taxa_liquida_matricula = c(
    "0 a 3 anos" = 60,
    "4 a 5 anos" = 98,
    "6 a 10 anos" = 99,
    "11 a 14 anos" = 98,
    "15 a 17 anos" = 95
  ),
  ano_target = 2036
)
```

Projete os indicadores por faixa etária:

```r
indicadores_projetados <- projetar_indicadores_matricula(
  indicadores_observados = indicadores_observados,
  populacao = populacao,
  metas = metas,
  anos = 2026:2036,
  ano_base = 2025
)

head(indicadores_projetados)
```

Componha a tabela final de matrículas por etapa de ensino:

```r
matriculas_etapa_projetadas <- compor_matriculas_etapa_projetadas(
  indicadores_projetados
)

head(matriculas_etapa_projetadas)
```

Para consultar uma UF específica em formato wide:

```r
matriculas_etapa_projetadas |>
  dplyr::filter(NO_UF == "Acre") |>
  dplyr::select(ANO, ETAPA_ENSINO, TOTAL_MATRICULAS) |>
  tidyr::pivot_wider(
    names_from = ETAPA_ENSINO,
    values_from = TOTAL_MATRICULAS
  )
```

## Desenvolvimento

Para atualizar os snapshots Parquet incluídos no pacote, configure o token do
MotherDuck, tenha o pacote `readxl` disponível para tratar o XLS de população
municipal e execute o script de preparação a partir da raiz do projeto:

```r
source("data-raw/gerar_dados_parquet.R")
```

O script gera:

```text
inst/extdata/matricula_faixaetaria.parquet
inst/extdata/matricula_faixaetaria_etapa.parquet
inst/extdata/projecao_populacional_ibge.parquet
inst/extdata/pop_municipio_2025.parquet
```

Rode os testes a partir da raiz do projeto:

```r
testthat::test_dir("tests/testthat")
```

Ou execute a checagem do pacote:

```r
R CMD check .
```

Para um tutorial mais detalhado, consulte a vignette:

```r
vignette("instalacao-e-uso", package = "ofertaDemandaDocente")
```
