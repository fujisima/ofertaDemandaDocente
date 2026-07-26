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
`ler_matricula_faixaetaria_etapa()` e `ler_projecao_populacional_ibge()` leem,
por padrão, snapshots em Parquet incluídos no pacote. Assim, depois da
instalação, é possível carregar os dados sem configurar conexão com o MotherDuck:

```r
matriculas <- ler_matricula_faixaetaria()
matriculas_etapa <- ler_matricula_faixaetaria_etapa()
populacao <- ler_projecao_populacional_ibge()
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

Crie metas gerais por etapa de ensino:

```r
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
```

Projete os indicadores e as matrículas:

```r
projecao <- projetar_indicadores_matricula(
  indicadores_observados = indicadores_observados,
  populacao = populacao,
  metas = metas,
  anos = 2026:2036,
  ano_base = 2025
)

head(projecao)
```

## Desenvolvimento

Para atualizar os snapshots Parquet incluídos no pacote, configure o token do
MotherDuck e execute o script de preparação a partir da raiz do projeto:

```r
source("data-raw/gerar_dados_parquet.R")
```

O script gera:

```text
inst/extdata/matricula_faixaetaria.parquet
inst/extdata/matricula_faixaetaria_etapa.parquet
inst/extdata/projecao_populacional_ibge.parquet
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
