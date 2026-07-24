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

## Configuração do MotherDuck

As funções `ler_matricula_faixaetaria()` e
`ler_projecao_populacional_ibge()` consultam tabelas no MotherDuck. Para usá-las,
configure um token no arquivo `.Renviron`:

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

Com o token configurado, leia os dados de origem:

```r
matriculas <- ler_matricula_faixaetaria()
populacao <- ler_projecao_populacional_ibge()
```

Em seguida, calcule os indicadores observados:

```r
indicadores_observados <- calcular_indicadores_matricula_observados(
  matriculas = matriculas,
  populacao = populacao,
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
