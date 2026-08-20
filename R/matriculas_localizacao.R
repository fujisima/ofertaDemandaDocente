ratear_matriculas_por_localizacao <- function(
  indicadores,
  populacao_localizacao,
  ano = 2025
) {
  colunas_matriculas <- c(
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II"
  )

  validar_colunas(
    indicadores,
    c(
      "ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA",
      "ETAPA_ENSINO_ADEQUADA", "ETAPA_ENSINO_ADEQUADA_NOME",
      colunas_matriculas
    ),
    "indicadores"
  )
  validar_colunas(
    populacao_localizacao,
    c(
      "ano_referencia", "trimestre_referencia", "UF", "faixa_etaria",
      "criterio_localizacao", "grupo_localizacao", "populacao_estimada",
      "erro_padrao", "coeficiente_variacao", "percentual_uf_faixa"
    ),
    "populacao_localizacao"
  )

  indicadores_ano <- indicadores |>
    dplyr::filter(ANO == ano)
  populacao_ano <- populacao_localizacao |>
    dplyr::filter(ano_referencia == ano) |>
    dplyr::mutate(
      UF = as.integer(as.character(UF)),
      SIGLA_UF = sigla_uf_ibge(UF)
    )

  if (nrow(indicadores_ano) == 0) {
    stop(sprintf("Nao ha indicadores para o ano de %s.", ano), call. = FALSE)
  }
  if (nrow(populacao_ano) == 0) {
    stop(sprintf("Nao ha populacao por localizacao para o ano de %s.", ano), call. = FALSE)
  }
  if (anyNA(populacao_ano$SIGLA_UF)) {
    stop("Ha codigos de UF desconhecidos em `populacao_localizacao`.", call. = FALSE)
  }

  chaves_populacao <- populacao_ano |>
    dplyr::count(
      SIGLA_UF,
      faixa_etaria,
      grupo_localizacao,
      name = "n"
    )
  if (any(chaves_populacao$n != 1)) {
    stop(
      paste(
        "`populacao_localizacao` deve ter uma linha por UF, faixa etaria",
        "e grupo de localizacao."
      ),
      call. = FALSE
    )
  }

  resultado <- indicadores_ano |>
    dplyr::select(
      ANO,
      SIGLA_UF,
      NO_UF,
      FAIXA_ETARIA,
      ETAPA_ENSINO_ADEQUADA,
      ETAPA_ENSINO_ADEQUADA_NOME,
      dplyr::all_of(colunas_matriculas)
    ) |>
    dplyr::left_join(
      populacao_ano |>
        dplyr::select(
          UF,
          SIGLA_UF,
          faixa_etaria,
          trimestre_referencia,
          criterio_localizacao,
          grupo_localizacao,
          populacao_estimada,
          erro_padrao,
          coeficiente_variacao,
          percentual_uf_faixa
        ),
      by = c(
        "SIGLA_UF",
        "FAIXA_ETARIA" = "faixa_etaria"
      )
    )

  if (anyNA(resultado$grupo_localizacao)) {
    faltantes <- resultado |>
      dplyr::filter(is.na(grupo_localizacao)) |>
      dplyr::distinct(SIGLA_UF, FAIXA_ETARIA)
    stop(
      sprintf(
        "Ha combinacoes sem populacao por localizacao: %s.",
        paste(
          paste(faltantes$SIGLA_UF, faltantes$FAIXA_ETARIA, sep = "/"),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(colunas_matriculas),
        ~ .x * percentual_uf_faixa / 100
      )
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      FAIXA_ETARIA,
      match(grupo_localizacao, c("Polo urbano", "Demais áreas"))
    )
}

ratear_matriculas_projetadas_por_localizacao <- function(
  indicadores_projetados,
  populacao_localizacao,
  ano_base_localizacao = 2025
) {
  colunas_matriculas <- c(
    "MAT_FAIXA_ADEQUADA",
    "MAT_AVANCADOS",
    "MAT_DEFASAGEM_I",
    "MAT_DEFASAGEM_II"
  )

  validar_colunas(
    indicadores_projetados,
    c("ANO", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA", colunas_matriculas),
    "indicadores_projetados"
  )
  validar_colunas(
    populacao_localizacao,
    c(
      "ano_referencia", "UF", "faixa_etaria", "criterio_localizacao",
      "grupo_localizacao", "percentual_uf_faixa"
    ),
    "populacao_localizacao"
  )

  percentuais_localizacao <- populacao_localizacao |>
    dplyr::filter(ano_referencia == ano_base_localizacao) |>
    dplyr::mutate(
      UF = as.integer(as.character(UF)),
      SIGLA_UF = sigla_uf_ibge(UF)
    )

  if (nrow(indicadores_projetados) == 0) {
    stop("`indicadores_projetados` nao possui linhas.", call. = FALSE)
  }
  chaves_indicadores <- indicadores_projetados |>
    dplyr::count(ANO, SIGLA_UF, FAIXA_ETARIA, name = "n")
  if (any(chaves_indicadores$n != 1)) {
    stop(
      paste(
        "`indicadores_projetados` deve ter uma linha por ano, UF e",
        "faixa etaria."
      ),
      call. = FALSE
    )
  }
  if (nrow(percentuais_localizacao) == 0) {
    stop(
      sprintf(
        "Nao ha populacao por localizacao para o ano de referencia %s.",
        ano_base_localizacao
      ),
      call. = FALSE
    )
  }
  if (anyNA(percentuais_localizacao$SIGLA_UF)) {
    stop("Ha codigos de UF desconhecidos em `populacao_localizacao`.", call. = FALSE)
  }
  if (anyNA(percentuais_localizacao$percentual_uf_faixa)) {
    stop("Ha percentuais de localizacao ausentes.", call. = FALSE)
  }

  chaves_percentuais <- percentuais_localizacao |>
    dplyr::count(
      SIGLA_UF,
      faixa_etaria,
      grupo_localizacao,
      name = "n"
    )
  if (any(chaves_percentuais$n != 1)) {
    stop(
      paste(
        "`populacao_localizacao` deve ter uma linha por UF, faixa etaria",
        "e grupo de localizacao no ano de referencia."
      ),
      call. = FALSE
    )
  }

  totais_percentuais <- percentuais_localizacao |>
    dplyr::group_by(SIGLA_UF, faixa_etaria) |>
    dplyr::summarise(
      numero_grupos = dplyr::n(),
      percentual_total = sum(percentual_uf_faixa),
      .groups = "drop"
    )
  if (
    any(totais_percentuais$numero_grupos != 2) ||
      any(abs(totais_percentuais$percentual_total - 100) > 1e-8)
  ) {
    stop(
      paste(
        "Os percentuais devem conter dois grupos e somar 100 para cada",
        "combinacao de UF e faixa etaria."
      ),
      call. = FALSE
    )
  }

  resultado <- indicadores_projetados |>
    dplyr::select(
      ANO,
      SIGLA_UF,
      NO_UF,
      FAIXA_ETARIA,
      dplyr::all_of(colunas_matriculas)
    ) |>
    dplyr::left_join(
      percentuais_localizacao |>
        dplyr::select(
          UF,
          SIGLA_UF,
          faixa_etaria,
          criterio_localizacao,
          grupo_localizacao,
          percentual_uf_faixa
        ),
      by = c(
        "SIGLA_UF",
        "FAIXA_ETARIA" = "faixa_etaria"
      ),
      relationship = "many-to-many"
    )

  if (anyNA(resultado$grupo_localizacao)) {
    faltantes <- resultado |>
      dplyr::filter(is.na(grupo_localizacao)) |>
      dplyr::distinct(SIGLA_UF, FAIXA_ETARIA)
    stop(
      sprintf(
        "Ha combinacoes sem percentual de localizacao: %s.",
        paste(
          paste(faltantes$SIGLA_UF, faltantes$FAIXA_ETARIA, sep = "/"),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(colunas_matriculas),
        ~ .x * percentual_uf_faixa / 100
      )
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      FAIXA_ETARIA,
      match(grupo_localizacao, c("Polo urbano", "Demais áreas"))
    )
}

compor_matriculas_etapa_projetadas_localizacao <- function(
  indicadores_projetados_localizacao,
  percentuais_em_localizacao,
  ano_referencia_composicao = 2025
) {
  validar_colunas(
    indicadores_projetados_localizacao,
    c(
      "ANO", "UF", "SIGLA_UF", "NO_UF", "FAIXA_ETARIA",
      "criterio_localizacao", "grupo_localizacao",
      "MAT_FAIXA_ADEQUADA", "MAT_AVANCADOS",
      "MAT_DEFASAGEM_I", "MAT_DEFASAGEM_II"
    ),
    "indicadores_projetados_localizacao"
  )
  validar_colunas(
    percentuais_em_localizacao,
    c(
      "ANO", "NO_UF", "GRUPO_LOCALIZACAO",
      "P_EM_PROPEDEUTICO", "P_EM_EPT"
    ),
    "percentuais_em_localizacao"
  )

  if (nrow(indicadores_projetados_localizacao) == 0) {
    stop("`indicadores_projetados_localizacao` nao possui linhas.", call. = FALSE)
  }

  metadados_uf <- indicadores_projetados_localizacao |>
    dplyr::distinct(
      UF,
      SIGLA_UF,
      NO_UF,
      criterio_localizacao,
      grupo_localizacao
    )
  chaves_metadados <- metadados_uf |>
    dplyr::count(
      SIGLA_UF,
      criterio_localizacao,
      grupo_localizacao,
      name = "n"
    )
  if (any(chaves_metadados$n != 1)) {
    stop(
      paste(
        "Cada combinacao de UF, criterio e grupo de localizacao deve",
        "corresponder a um unico codigo e nome de UF."
      ),
      call. = FALSE
    )
  }

  chave_grupo <- interaction(
    indicadores_projetados_localizacao$criterio_localizacao,
    indicadores_projetados_localizacao$grupo_localizacao,
    drop = TRUE,
    lex.order = TRUE
  )
  dados_por_grupo <- split(indicadores_projetados_localizacao, chave_grupo)

  resultados <- lapply(dados_por_grupo, function(dados_grupo) {
    criterio <- unique(dados_grupo$criterio_localizacao)
    grupo <- unique(dados_grupo$grupo_localizacao)

    resultado_grupo <- compor_matriculas_etapa_projetadas(dados_grupo)
    metadados_grupo <- metadados_uf[
      metadados_uf$criterio_localizacao == criterio &
        metadados_uf$grupo_localizacao == grupo,
    ]

    merge(
      resultado_grupo,
      metadados_grupo,
      by = c("SIGLA_UF", "NO_UF"),
      all.x = TRUE,
      all.y = FALSE
    )
  })

  resultado <- do.call(rbind, resultados) |>
    dplyr::mutate(
      grupo_localizacao = dplyr::recode(
        grupo_localizacao,
        "Polo urbano" = "Capital",
        "Demais áreas" = "Interior"
      )
    )

  detalhar_ensino_medio_localizacao(
    matriculas_etapa = resultado,
    percentuais_em_localizacao = percentuais_em_localizacao,
    ano_referencia_composicao = ano_referencia_composicao
  )
}

detalhar_ensino_medio_localizacao <- function(
  matriculas_etapa,
  percentuais_em_localizacao,
  ano_referencia_composicao
) {
  percentuais_referencia <- percentuais_em_localizacao |>
    dplyr::filter(ANO == ano_referencia_composicao) |>
    dplyr::select(
      NO_UF,
      GRUPO_LOCALIZACAO,
      P_EM_PROPEDEUTICO,
      P_EM_EPT
    )

  if (nrow(percentuais_referencia) == 0) {
    stop(
      sprintf(
        "Nao ha percentuais de composicao do ensino medio para %s.",
        ano_referencia_composicao
      ),
      call. = FALSE
    )
  }

  chaves_percentuais <- percentuais_referencia |>
    dplyr::count(NO_UF, GRUPO_LOCALIZACAO, name = "n")
  if (any(chaves_percentuais$n != 1)) {
    stop(
      paste(
        "`percentuais_em_localizacao` deve ter uma linha por UF e grupo",
        "de localizacao no ano de referencia."
      ),
      call. = FALSE
    )
  }

  pares_incompletos <- xor(
    is.na(percentuais_referencia$P_EM_PROPEDEUTICO),
    is.na(percentuais_referencia$P_EM_EPT)
  )
  if (any(pares_incompletos)) {
    stop(
      paste(
        "Os percentuais de propedeutico e EPT devem estar ambos",
        "preenchidos ou ambos ausentes."
      ),
      call. = FALSE
    )
  }

  percentuais_disponiveis <- percentuais_referencia |>
    dplyr::filter(!is.na(P_EM_PROPEDEUTICO), !is.na(P_EM_EPT))
  if (
    any(percentuais_disponiveis$P_EM_PROPEDEUTICO < 0) ||
      any(percentuais_disponiveis$P_EM_PROPEDEUTICO > 1) ||
      any(percentuais_disponiveis$P_EM_EPT < 0) ||
      any(percentuais_disponiveis$P_EM_EPT > 1) ||
      any(abs(
        percentuais_disponiveis$P_EM_PROPEDEUTICO +
          percentuais_disponiveis$P_EM_EPT - 1
      ) > 1e-8)
  ) {
    stop(
      "Os percentuais de propedeutico e EPT devem estar entre 0 e 1 e somar 1.",
      call. = FALSE
    )
  }

  outras_etapas <- matriculas_etapa |>
    dplyr::filter(ETAPA_ENSINO != "EM") |>
    dplyr::mutate(
      ETAPA_ENSINO_DETALHE = ETAPA_ENSINO,
      ETAPA_ENSINO_DETALHE_NOME = ETAPA_ENSINO_NOME,
      P_COMPOSICAO = 1,
      ANO_REFERENCIA_COMPOSICAO = NA_integer_,
      FONTE_COMPOSICAO = NA_character_
    )

  ensino_medio <- matriculas_etapa |>
    dplyr::filter(ETAPA_ENSINO == "EM") |>
    dplyr::left_join(
      percentuais_referencia,
      by = c(
        "NO_UF",
        "grupo_localizacao" = "GRUPO_LOCALIZACAO"
      ),
      relationship = "many-to-one"
    )

  percentuais_faltantes <- ensino_medio |>
    dplyr::filter(
      TOTAL_MATRICULAS > 0,
      is.na(P_EM_PROPEDEUTICO) | is.na(P_EM_EPT)
    ) |>
    dplyr::distinct(NO_UF, grupo_localizacao)

  if (nrow(percentuais_faltantes) > 0) {
    chaves_faltantes <- paste(
      percentuais_faltantes$NO_UF,
      percentuais_faltantes$grupo_localizacao,
      sep = "/"
    )
    stop(
      sprintf(
        paste(
          "Ha matriculas projetadas positivas sem percentual de composicao",
          "do ensino medio: %s."
        ),
        paste(chaves_faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  ensino_medio_proped <- ensino_medio |>
    dplyr::mutate(
      ETAPA_ENSINO_DETALHE = "EM_PROP",
      ETAPA_ENSINO_DETALHE_NOME = "Ensino medio propedeutico",
      P_COMPOSICAO = P_EM_PROPEDEUTICO,
      ANO_REFERENCIA_COMPOSICAO = as.integer(ano_referencia_composicao),
      FONTE_COMPOSICAO = sprintf(
        "Censo Escolar %s",
        ano_referencia_composicao
      ),
      TOTAL_MATRICULAS = dplyr::if_else(
        TOTAL_MATRICULAS == 0 & is.na(P_COMPOSICAO),
        0,
        TOTAL_MATRICULAS * P_COMPOSICAO
      )
    )

  ensino_medio_ept <- ensino_medio |>
    dplyr::mutate(
      ETAPA_ENSINO_DETALHE = "EM_EPT",
      ETAPA_ENSINO_DETALHE_NOME = "Ensino medio EPT",
      P_COMPOSICAO = P_EM_EPT,
      ANO_REFERENCIA_COMPOSICAO = as.integer(ano_referencia_composicao),
      FONTE_COMPOSICAO = sprintf(
        "Censo Escolar %s",
        ano_referencia_composicao
      ),
      TOTAL_MATRICULAS = dplyr::if_else(
        TOTAL_MATRICULAS == 0 & is.na(P_COMPOSICAO),
        0,
        TOTAL_MATRICULAS * P_COMPOSICAO
      )
    )

  colunas_resultado <- c(
    "ANO",
    "UF",
    "SIGLA_UF",
    "NO_UF",
    "criterio_localizacao",
    "grupo_localizacao",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "ETAPA_ENSINO_DETALHE",
    "ETAPA_ENSINO_DETALHE_NOME",
    "P_COMPOSICAO",
    "ANO_REFERENCIA_COMPOSICAO",
    "FONTE_COMPOSICAO",
    "TOTAL_MATRICULAS"
  )

  resultado <- dplyr::bind_rows(
    outras_etapas,
    ensino_medio_proped,
    ensino_medio_ept
  ) |>
    dplyr::select(dplyr::all_of(colunas_resultado)) |>
    dplyr::mutate(
      ordem_etapa = match(
        ETAPA_ENSINO,
        c("CRE", "PRE", "AI", "AF", "EM")
      ),
      ordem_detalhe = match(
        ETAPA_ENSINO_DETALHE,
        c("CRE", "PRE", "AI", "AF", "EM_PROP", "EM_EPT")
      ),
      ordem_grupo = match(
        grupo_localizacao,
        c("Capital", "Interior")
      )
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      ordem_etapa,
      ordem_detalhe,
      ordem_grupo
    ) |>
    dplyr::select(-ordem_etapa, -ordem_detalhe, -ordem_grupo)

  row.names(resultado) <- NULL
  resultado
}

sigla_uf_ibge <- function(codigo_uf) {
  codigos <- c(
    11, 12, 13, 14, 15, 16, 17,
    21, 22, 23, 24, 25, 26, 27, 28, 29,
    31, 32, 33, 35,
    41, 42, 43,
    50, 51, 52, 53
  )
  siglas <- c(
    "RO", "AC", "AM", "RR", "PA", "AP", "TO",
    "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
    "MG", "ES", "RJ", "SP",
    "PR", "SC", "RS",
    "MS", "MT", "GO", "DF"
  )

  siglas[match(codigo_uf, codigos)]
}
