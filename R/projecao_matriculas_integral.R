criar_metas_percentual_integral <- function(
  percentual_integral,
  incremento_acima_meta = 10,
  ano_target = 2036,
  limite = 100
) {
  if (is.null(names(percentual_integral)) || any(!nzchar(names(percentual_integral)))) {
    stop(
      "`percentual_integral` deve ser nomeado por etapa de ensino detalhada.",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(percentual_integral))) {
    stop("Ha etapas duplicadas em `percentual_integral`.", call. = FALSE)
  }

  etapas <- names(percentual_integral)
  expandir_parametro <- function(valor, nome) {
    if (length(valor) == 1) {
      return(rep(as.numeric(valor), length(etapas)))
    }
    if (is.null(names(valor)) || !setequal(names(valor), etapas)) {
      stop(
        sprintf(
          "`%s` deve ter comprimento 1 ou ser nomeado pelas mesmas etapas.",
          nome
        ),
        call. = FALSE
      )
    }
    as.numeric(valor[etapas])
  }

  meta <- as.numeric(percentual_integral)
  incremento <- expandir_parametro(
    incremento_acima_meta,
    "incremento_acima_meta"
  )
  limites <- expandir_parametro(limite, "limite")

  if (
    length(ano_target) != 1 ||
      !is.numeric(ano_target) ||
      is.na(ano_target) ||
      !is.finite(ano_target)
  ) {
    stop("`ano_target` deve ser um unico ano numerico e finito.", call. = FALSE)
  }
  if (
    anyNA(meta) || anyNA(incremento) || anyNA(limites) ||
      any(!is.finite(meta)) ||
      any(!is.finite(incremento)) ||
      any(!is.finite(limites)) ||
      any(meta < 0) ||
      any(incremento < 0) ||
      any(limites <= 0) ||
      any(limites > 100) ||
      any(meta > limites)
  ) {
    stop(
      paste(
        "Metas, incrementos e limites devem ser finitos e nao negativos;",
        "os limites nao podem superar 100 e cada meta deve respeitar o limite."
      ),
      call. = FALSE
    )
  }

  data.frame(
    ETAPA_ENSINO_DETALHE = etapas,
    ANO_TARGET = as.integer(ano_target),
    META_PERCENTUAL_INTEGRAL = meta,
    INCREMENTO_ACIMA_META = incremento,
    LIMITE_PERCENTUAL_INTEGRAL = limites,
    stringsAsFactors = FALSE
  )
}

projetar_percentuais_matriculas_integral <- function(
  percentuais_observados,
  metas,
  anos = 2026:2036,
  ano_base = 2025
) {
  chaves <- c(
    "NO_UF",
    "GRUPO_LOCALIZACAO",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME",
    "ETAPA_ENSINO_DETALHE",
    "ETAPA_ENSINO_DETALHE_NOME"
  )
  validar_colunas(
    percentuais_observados,
    c("ANO", chaves, "P_INTEGRAL", "P_PARCIAL"),
    "percentuais_observados"
  )
  validar_colunas(
    metas,
    c(
      "ETAPA_ENSINO_DETALHE", "ANO_TARGET",
      "META_PERCENTUAL_INTEGRAL", "INCREMENTO_ACIMA_META",
      "LIMITE_PERCENTUAL_INTEGRAL"
    ),
    "metas"
  )

  if (
    length(ano_base) != 1 ||
      !is.numeric(ano_base) ||
      is.na(ano_base) ||
      !is.finite(ano_base)
  ) {
    stop("`ano_base` deve ser um unico ano numerico e finito.", call. = FALSE)
  }
  if (
    !is.numeric(anos) || anyNA(anos) || any(!is.finite(anos)) ||
      any(anos <= ano_base) || anyDuplicated(anos)
  ) {
    stop("`anos` deve conter anos numericos posteriores ao ano-base.", call. = FALSE)
  }

  observados_base <- percentuais_observados |>
    dplyr::filter(ANO == ano_base)
  if (nrow(observados_base) == 0) {
    stop(
      sprintf("Nao ha percentuais integrais observados para %s.", ano_base),
      call. = FALSE
    )
  }

  chaves_observadas <- observados_base |>
    dplyr::group_by(dplyr::across(dplyr::all_of(chaves))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  if (any(chaves_observadas$n != 1)) {
    stop(
      "Os percentuais observados devem ter uma linha por UF, grupo e etapa.",
      call. = FALSE
    )
  }

  pares_incompletos <- xor(
    is.na(observados_base$P_INTEGRAL),
    is.na(observados_base$P_PARCIAL)
  )
  percentuais_disponiveis <- !is.na(observados_base$P_INTEGRAL)
  if (
    any(pares_incompletos) ||
      any(observados_base$P_INTEGRAL[percentuais_disponiveis] < 0) ||
      any(observados_base$P_INTEGRAL[percentuais_disponiveis] > 1) ||
      any(abs(
        observados_base$P_INTEGRAL[percentuais_disponiveis] +
          observados_base$P_PARCIAL[percentuais_disponiveis] - 1
      ) > 1e-8)
  ) {
    stop(
      "Os percentuais observados devem estar entre 0 e 1 e somar 1.",
      call. = FALSE
    )
  }

  chaves_metas <- metas |>
    dplyr::count(ETAPA_ENSINO_DETALHE, name = "n")
  if (any(chaves_metas$n != 1)) {
    stop("As metas devem ter uma linha por etapa detalhada.", call. = FALSE)
  }
  if (
    anyNA(metas$META_PERCENTUAL_INTEGRAL) ||
      anyNA(metas$INCREMENTO_ACIMA_META) ||
      anyNA(metas$LIMITE_PERCENTUAL_INTEGRAL) ||
      any(metas$META_PERCENTUAL_INTEGRAL < 0) ||
      any(metas$INCREMENTO_ACIMA_META < 0) ||
      any(metas$LIMITE_PERCENTUAL_INTEGRAL <= 0) ||
      any(metas$LIMITE_PERCENTUAL_INTEGRAL > 100) ||
      any(
        metas$META_PERCENTUAL_INTEGRAL >
          metas$LIMITE_PERCENTUAL_INTEGRAL
      )
  ) {
    stop("A tabela de metas possui parametros percentuais invalidos.", call. = FALSE)
  }

  base_com_metas <- observados_base |>
    dplyr::left_join(
      metas,
      by = "ETAPA_ENSINO_DETALHE",
      relationship = "many-to-one"
    )
  if (anyNA(base_com_metas$ANO_TARGET)) {
    etapas_sem_meta <- unique(
      base_com_metas$ETAPA_ENSINO_DETALHE[is.na(base_com_metas$ANO_TARGET)]
    )
    stop(
      sprintf(
        "Ha etapas sem meta de percentual integral: %s.",
        paste(etapas_sem_meta, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(anos > min(base_com_metas$ANO_TARGET))) {
    stop("Os anos projetados nao podem ultrapassar o ano da meta.", call. = FALSE)
  }
  if (any(base_com_metas$ANO_TARGET <= ano_base)) {
    stop("O ano da meta deve ser posterior ao ano-base.", call. = FALSE)
  }

  base_com_metas <- base_com_metas |>
    dplyr::mutate(
      META_PROPORCAO_INTEGRAL = META_PERCENTUAL_INTEGRAL / 100,
      INCREMENTO_PROPORCAO = INCREMENTO_ACIMA_META / 100,
      LIMITE_PROPORCAO_INTEGRAL = LIMITE_PERCENTUAL_INTEGRAL / 100,
      P_INTEGRAL_TARGET = dplyr::case_when(
        is.na(P_INTEGRAL) ~ NA_real_,
        P_INTEGRAL < META_PROPORCAO_INTEGRAL ~ META_PROPORCAO_INTEGRAL,
        TRUE ~ pmin(
          P_INTEGRAL + INCREMENTO_PROPORCAO,
          LIMITE_PROPORCAO_INTEGRAL
        )
      ),
      P_INTEGRAL_BASE = P_INTEGRAL
    )

  tidyr::crossing(
    base_com_metas,
    ANO_PROJETADO = as.integer(anos)
  ) |>
    dplyr::mutate(
      ANO = ANO_PROJETADO,
      P_INTEGRAL = P_INTEGRAL_BASE +
        (P_INTEGRAL_TARGET - P_INTEGRAL_BASE) *
          (ANO - ano_base) / (ANO_TARGET - ano_base),
      P_PARCIAL = 1 - P_INTEGRAL
    ) |>
    dplyr::select(
      ANO,
      dplyr::all_of(chaves),
      P_INTEGRAL,
      P_PARCIAL,
      P_INTEGRAL_BASE,
      P_INTEGRAL_TARGET,
      ANO_TARGET,
      META_PERCENTUAL_INTEGRAL,
      INCREMENTO_ACIMA_META,
      LIMITE_PERCENTUAL_INTEGRAL
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      ETAPA_ENSINO,
      ETAPA_ENSINO_DETALHE,
      match(GRUPO_LOCALIZACAO, c("Capital", "Interior"))
    )
}

ratear_matriculas_projetadas_por_jornada <- function(
  matriculas_etapa_projetadas_localizacao,
  percentuais_integral_projetados,
  ano_referencia = 2025
) {
  validar_colunas(
    matriculas_etapa_projetadas_localizacao,
    c(
      "ANO", "NO_UF", "grupo_localizacao", "ETAPA_ENSINO",
      "ETAPA_ENSINO_DETALHE", "TOTAL_MATRICULAS"
    ),
    "matriculas_etapa_projetadas_localizacao"
  )
  validar_colunas(
    percentuais_integral_projetados,
    c(
      "ANO", "NO_UF", "GRUPO_LOCALIZACAO", "ETAPA_ENSINO",
      "ETAPA_ENSINO_DETALHE", "P_INTEGRAL", "P_PARCIAL",
      "P_INTEGRAL_BASE", "P_INTEGRAL_TARGET", "ANO_TARGET",
      "META_PERCENTUAL_INTEGRAL", "INCREMENTO_ACIMA_META",
      "LIMITE_PERCENTUAL_INTEGRAL"
    ),
    "percentuais_integral_projetados"
  )

  chaves_percentuais <- percentuais_integral_projetados |>
    dplyr::count(
      ANO,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      ETAPA_ENSINO_DETALHE,
      name = "n"
    )
  if (any(chaves_percentuais$n != 1)) {
    stop(
      paste(
        "Os percentuais projetados devem ter uma linha por ano, UF, grupo,",
        "etapa e detalhamento."
      ),
      call. = FALSE
    )
  }

  chaves_join <- c(
    "ANO",
    "NO_UF",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_DETALHE",
    "grupo_localizacao" = "GRUPO_LOCALIZACAO"
  )
  matriculas_com_percentuais <- matriculas_etapa_projetadas_localizacao |>
    dplyr::left_join(
      percentuais_integral_projetados |>
        dplyr::select(
          ANO,
          NO_UF,
          GRUPO_LOCALIZACAO,
          ETAPA_ENSINO,
          ETAPA_ENSINO_DETALHE,
          P_INTEGRAL,
          P_PARCIAL,
          P_INTEGRAL_BASE,
          P_INTEGRAL_TARGET,
          ANO_TARGET,
          META_PERCENTUAL_INTEGRAL,
          INCREMENTO_ACIMA_META,
          LIMITE_PERCENTUAL_INTEGRAL
        ),
      by = chaves_join,
      relationship = "many-to-one"
    )

  percentuais_faltantes <- matriculas_com_percentuais |>
    dplyr::filter(
      TOTAL_MATRICULAS > 0,
      is.na(P_INTEGRAL) | is.na(P_PARCIAL)
    ) |>
    dplyr::distinct(
      ANO,
      NO_UF,
      grupo_localizacao,
      ETAPA_ENSINO_DETALHE
    )
  if (nrow(percentuais_faltantes) > 0) {
    stop(
      paste(
        "Ha matriculas projetadas positivas sem percentual de jornada",
        "correspondente."
      ),
      call. = FALSE
    )
  }

  criar_jornada <- function(dados, tipo, coluna_percentual) {
    percentual <- dados[[coluna_percentual]]
    dados |>
      dplyr::mutate(
        TIPO_JORNADA = tipo,
        P_JORNADA = percentual,
        ANO_REFERENCIA_JORNADA = as.integer(ano_referencia),
        FONTE_JORNADA = "Censo Escolar 2025 e projecao linear",
        TOTAL_MATRICULAS = dplyr::if_else(
          TOTAL_MATRICULAS == 0 & is.na(P_JORNADA),
          0,
          TOTAL_MATRICULAS * P_JORNADA
        )
      )
  }

  resultado <- dplyr::bind_rows(
    criar_jornada(matriculas_com_percentuais, "Integral", "P_INTEGRAL"),
    criar_jornada(matriculas_com_percentuais, "Parcial", "P_PARCIAL")
  ) |>
    dplyr::select(-P_INTEGRAL, -P_PARCIAL) |>
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
      ),
      ordem_jornada = match(TIPO_JORNADA, c("Integral", "Parcial"))
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      ordem_etapa,
      ordem_detalhe,
      ordem_grupo,
      ordem_jornada
    ) |>
    dplyr::select(
      -ordem_etapa,
      -ordem_detalhe,
      -ordem_grupo,
      -ordem_jornada
    )

  row.names(resultado) <- NULL
  resultado
}

resumir_matriculas_projetadas <- function(
  matriculas_projetadas,
  media_alunos_turma_localizacao,
  ano_referencia_atu = 2025
) {
  colunas_resumo <- c(
    "ANO",
    "UF",
    "SIGLA_UF",
    "NO_UF",
    "grupo_localizacao",
    "ETAPA_ENSINO",
    "ETAPA_ENSINO_DETALHE",
    "P_COMPOSICAO",
    "TIPO_JORNADA",
    "P_JORNADA",
    "TOTAL_MATRICULAS"
  )

  validar_colunas(
    matriculas_projetadas,
    colunas_resumo,
    "matriculas_projetadas"
  )
  validar_colunas(
    media_alunos_turma_localizacao,
    c(
      "ANO", "SIGLA_UF", "NO_UF", "GRUPO_LOCALIZACAO",
      "ETAPA_ENSINO", "MEDIA_ALUNOS_TURMA"
    ),
    "media_alunos_turma_localizacao"
  )

  atu_referencia <- media_alunos_turma_localizacao |>
    dplyr::filter(ANO == ano_referencia_atu) |>
    dplyr::select(
      SIGLA_UF,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      MEDIA_ALUNOS_TURMA
    )

  if (nrow(atu_referencia) == 0) {
    stop(
      sprintf(
        "Nao ha medias de alunos por turma para o ano de referencia %s.",
        ano_referencia_atu
      ),
      call. = FALSE
    )
  }

  chaves_atu <- atu_referencia |>
    dplyr::count(
      SIGLA_UF,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      name = "n"
    ) |>
    dplyr::filter(n != 1)

  if (nrow(chaves_atu) > 0) {
    stop(
      paste(
        "As medias de alunos por turma devem ter uma linha por UF, grupo",
        "de localizacao e etapa no ano de referencia."
      ),
      call. = FALSE
    )
  }

  resultado <- matriculas_projetadas |>
    dplyr::select(dplyr::all_of(colunas_resumo)) |>
    dplyr::rename(
      P_ETAPA_DETALHE = P_COMPOSICAO,
      QT_MATRICULA = TOTAL_MATRICULAS
    ) |>
    dplyr::left_join(
      atu_referencia,
      by = c(
        "SIGLA_UF",
        "NO_UF",
        "grupo_localizacao" = "GRUPO_LOCALIZACAO",
        "ETAPA_ENSINO"
      ),
      relationship = "many-to-one"
    )

  atu_faltante <- resultado |>
    dplyr::filter(QT_MATRICULA > 0, is.na(MEDIA_ALUNOS_TURMA)) |>
    dplyr::distinct(
      SIGLA_UF,
      NO_UF,
      grupo_localizacao,
      ETAPA_ENSINO
    )

  if (nrow(atu_faltante) > 0) {
    stop(
      "Ha matriculas projetadas positivas sem media de alunos por turma.",
      call. = FALSE
    )
  }

  resultado
}
