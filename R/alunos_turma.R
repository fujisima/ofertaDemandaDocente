ler_atu_municipio_2025 <- function(
  path = "data-raw/ATU_2025_MUNICIPIOS/ATU_MUNICIPIOS_2025.xlsx"
) {
  if (!file.exists(path)) {
    stop(sprintf("Arquivo `%s` nao encontrado.", path), call. = FALSE)
  }

  dados <- readxl::read_xlsx(
    path,
    skip = 8,
    col_types = "text"
  )

  colunas_atu <- c(
    "CRE_CAT_0",
    "PRE_CAT_0",
    "FUN_AI_CAT_0",
    "FUN_AF_CAT_0",
    "MED_CAT_0"
  )

  validar_colunas(
    dados,
    c(
      "NU_ANO_CENSO", "SG_UF", "CO_MUNICIPIO", "NO_MUNICIPIO",
      "NO_CATEGORIA", "NO_DEPENDENCIA", colunas_atu
    ),
    "atu_municipio"
  )

  dados <- dados |>
    dplyr::filter(
      NU_ANO_CENSO == "2025",
      NO_CATEGORIA == "Total",
      NO_DEPENDENCIA == "Total"
    ) |>
    dplyr::select(
      NU_ANO_CENSO,
      SG_UF,
      CO_MUNICIPIO,
      NO_MUNICIPIO,
      dplyr::all_of(colunas_atu)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(colunas_atu),
      names_to = "COLUNA_ATU",
      values_to = "MEDIA_ALUNOS_TURMA_TEXTO"
    )

  if (nrow(dados) == 0) {
    stop("Nao ha dados municipais de ATU para Total/Total.", call. = FALSE)
  }

  valores_texto <- trimws(as.character(dados$MEDIA_ALUNOS_TURMA_TEXTO))
  valores_ausentes <- is.na(valores_texto) |
    valores_texto == "" |
    valores_texto == "--"
  valores_numericos <- suppressWarnings(as.numeric(valores_texto))

  if (any(!valores_ausentes & is.na(valores_numericos))) {
    valores_invalidos <- unique(
      valores_texto[!valores_ausentes & is.na(valores_numericos)]
    )
    stop(
      sprintf(
        "Ha valores de ATU nao numericos: %s.",
        paste(valores_invalidos, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  valores_numericos[valores_ausentes] <- NA_real_

  resultado <- dados |>
    dplyr::transmute(
      ANO = suppressWarnings(as.integer(NU_ANO_CENSO)),
      SIGLA_UF = as.character(SG_UF),
      COD_MUN = as.character(CO_MUNICIPIO),
      NO_MUNICIPIO = as.character(NO_MUNICIPIO),
      ETAPA_ENSINO = dplyr::recode(
        COLUNA_ATU,
        "CRE_CAT_0" = "CRE",
        "PRE_CAT_0" = "PRE",
        "FUN_AI_CAT_0" = "AI",
        "FUN_AF_CAT_0" = "AF",
        "MED_CAT_0" = "EM"
      ),
      MEDIA_ALUNOS_TURMA = valores_numericos
    )

  if (
    anyNA(resultado$ANO) ||
      anyNA(resultado$SIGLA_UF) ||
      anyNA(resultado$COD_MUN) ||
      any(resultado$SIGLA_UF == "") ||
      any(resultado$COD_MUN == "")
  ) {
    stop(
      "Ha ano, UF ou codigo de municipio ausente nos dados de ATU.",
      call. = FALSE
    )
  }

  if (any(
    !is.na(resultado$MEDIA_ALUNOS_TURMA) &
      resultado$MEDIA_ALUNOS_TURMA <= 0
  )) {
    stop("As medias de alunos por turma devem ser positivas.", call. = FALSE)
  }

  duplicados <- resultado |>
    dplyr::count(ANO, COD_MUN, ETAPA_ENSINO, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicados) > 0) {
    stop(
      "Ha mais de um ATU por ano, municipio e etapa de ensino.",
      call. = FALSE
    )
  }

  resultado |>
    dplyr::arrange(
      ANO,
      SIGLA_UF,
      COD_MUN,
      match(ETAPA_ENSINO, c("CRE", "PRE", "AI", "AF", "EM"))
    )
}

calcular_media_alunos_turma_localizacao <- function(
  atu_municipio,
  matriculas_turno_municipio,
  matriculas_em_municipio,
  municipios_capital_rm
) {
  validar_colunas(
    atu_municipio,
    c(
      "ANO", "SIGLA_UF", "COD_MUN", "ETAPA_ENSINO",
      "MEDIA_ALUNOS_TURMA"
    ),
    "atu_municipio"
  )
  validar_colunas(
    matriculas_turno_municipio,
    c(
      "ANO", "NO_UF", "COD_MUN", "ETAPA_ENSINO",
      "QT_MAT_INTEGRAL", "QT_MAT_PARCIAL"
    ),
    "matriculas_turno_municipio"
  )
  validar_colunas(
    matriculas_em_municipio,
    c(
      "ANO", "NO_UF", "COD_MUN", "QT_MAT_EM_TOTAL"
    ),
    "matriculas_em_municipio"
  )
  validar_colunas(
    municipios_capital_rm,
    "COD_MUN",
    "municipios_capital_rm"
  )

  colunas_matriculas_turno <- c("QT_MAT_INTEGRAL", "QT_MAT_PARCIAL")
  valores_matriculas <- c(
    unlist(matriculas_turno_municipio[colunas_matriculas_turno]),
    matriculas_em_municipio$QT_MAT_EM_TOTAL
  )

  if (anyNA(valores_matriculas) || any(valores_matriculas < 0)) {
    stop("As quantidades de matriculas devem ser nao negativas.", call. = FALSE)
  }

  matriculas_etapas_gerais <- matriculas_turno_municipio |>
    dplyr::transmute(
      ANO = as.integer(ANO),
      NO_UF = as.character(NO_UF),
      COD_MUN = as.character(COD_MUN),
      ETAPA_ENSINO = as.character(ETAPA_ENSINO),
      QT_MAT_TOTAL = QT_MAT_INTEGRAL + QT_MAT_PARCIAL
    )

  matriculas_em <- matriculas_em_municipio |>
    dplyr::transmute(
      ANO = as.integer(ANO),
      NO_UF = as.character(NO_UF),
      COD_MUN = as.character(COD_MUN),
      ETAPA_ENSINO = "EM",
      QT_MAT_TOTAL = QT_MAT_EM_TOTAL
    )

  matriculas_municipio <- dplyr::bind_rows(
    matriculas_etapas_gerais,
    matriculas_em
  ) |>
    dplyr::group_by(ANO, NO_UF, COD_MUN, ETAPA_ENSINO) |>
    dplyr::summarise(
      QT_MAT_TOTAL = sum(QT_MAT_TOTAL),
      .groups = "drop"
    )

  etapas_esperadas <- c("CRE", "PRE", "AI", "AF", "EM")
  etapas_turno_esperadas <- c("CRE", "PRE", "AI", "AF")
  etapas_turno_desconhecidas <- setdiff(
    unique(matriculas_etapas_gerais$ETAPA_ENSINO),
    etapas_turno_esperadas
  )

  if (length(etapas_turno_desconhecidas) > 0) {
    stop(
      sprintf(
        "Ha etapas de ensino desconhecidas nas matriculas por turno: %s.",
        paste(etapas_turno_desconhecidas, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  etapas_desconhecidas <- setdiff(
    unique(matriculas_municipio$ETAPA_ENSINO),
    etapas_esperadas
  )

  if (length(etapas_desconhecidas) > 0) {
    stop(
      sprintf(
        "Ha etapas de ensino desconhecidas nas matriculas: %s.",
        paste(etapas_desconhecidas, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  atu_chaves <- atu_municipio |>
    dplyr::mutate(
      ANO = as.integer(ANO),
      COD_MUN = as.character(COD_MUN),
      ETAPA_ENSINO = as.character(ETAPA_ENSINO)
    ) |>
    dplyr::select(
      ANO,
      SIGLA_UF,
      COD_MUN,
      ETAPA_ENSINO,
      MEDIA_ALUNOS_TURMA
    )

  duplicados_atu <- atu_chaves |>
    dplyr::count(ANO, COD_MUN, ETAPA_ENSINO, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicados_atu) > 0) {
    stop(
      "`atu_municipio` deve ter uma linha por ano, municipio e etapa.",
      call. = FALSE
    )
  }

  etapas_atu_desconhecidas <- setdiff(
    unique(atu_chaves$ETAPA_ENSINO),
    etapas_esperadas
  )

  if (length(etapas_atu_desconhecidas) > 0) {
    stop(
      sprintf(
        "Ha etapas de ensino desconhecidas nos dados de ATU: %s.",
        paste(etapas_atu_desconhecidas, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dados <- matriculas_municipio |>
    dplyr::left_join(
      atu_chaves,
      by = c("ANO", "COD_MUN", "ETAPA_ENSINO")
    )

  if (anyNA(dados$SIGLA_UF)) {
    stop(
      "Ha combinacoes de ano, municipio e etapa sem registro municipal de ATU.",
      call. = FALSE
    )
  }

  atu_positivo_sem_matricula <- atu_chaves |>
    dplyr::filter(!is.na(MEDIA_ALUNOS_TURMA)) |>
    dplyr::anti_join(
      matriculas_municipio,
      by = c("ANO", "COD_MUN", "ETAPA_ENSINO")
    )

  if (nrow(atu_positivo_sem_matricula) > 0) {
    stop(
      "Ha ATU municipal positivo sem registro correspondente de matriculas.",
      call. = FALSE
    )
  }

  if (any(
    !is.na(dados$MEDIA_ALUNOS_TURMA) &
      dados$MEDIA_ALUNOS_TURMA <= 0
  )) {
    stop("As medias de alunos por turma devem ser positivas.", call. = FALSE)
  }

  codigos_capital_rm <- unique(as.character(municipios_capital_rm$COD_MUN))

  dados <- dados |>
    dplyr::mutate(
      GRUPO_LOCALIZACAO = dplyr::if_else(
        COD_MUN %in% codigos_capital_rm,
        "Capital",
        "Interior"
      )
    )

  atu_grupo_observado <- dados |>
    dplyr::filter(QT_MAT_TOTAL > 0, !is.na(MEDIA_ALUNOS_TURMA)) |>
    dplyr::group_by(
      ANO,
      SIGLA_UF,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO
    ) |>
    dplyr::summarise(
      MEDIA_ALUNOS_TURMA_GRUPO = sum(QT_MAT_TOTAL) /
        sum(QT_MAT_TOTAL / MEDIA_ALUNOS_TURMA),
      .groups = "drop"
    )

  dados <- dados |>
    dplyr::left_join(
      atu_grupo_observado,
      by = c(
        "ANO",
        "SIGLA_UF",
        "NO_UF",
        "GRUPO_LOCALIZACAO",
        "ETAPA_ENSINO"
      ),
      relationship = "many-to-one"
    )

  sem_atu_referencia <- dados |>
    dplyr::filter(
      QT_MAT_TOTAL > 0,
      is.na(MEDIA_ALUNOS_TURMA),
      is.na(MEDIA_ALUNOS_TURMA_GRUPO)
    )

  if (nrow(sem_atu_referencia) > 0) {
    exemplos <- sem_atu_referencia |>
      dplyr::transmute(chave = paste(NO_UF, COD_MUN, ETAPA_ENSINO, sep = "/")) |>
      dplyr::slice_head(n = 10) |>
      dplyr::pull(chave)
    stop(
      sprintf(
        "Ha matriculas positivas sem ATU municipal ou referencia do grupo: %s.",
        paste(exemplos, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  resultado <- dados |>
    dplyr::mutate(
      ATU_IMPUTADO = QT_MAT_TOTAL > 0 & is.na(MEDIA_ALUNOS_TURMA),
      MEDIA_ALUNOS_TURMA_CALCULO = dplyr::if_else(
        ATU_IMPUTADO,
        MEDIA_ALUNOS_TURMA_GRUPO,
        MEDIA_ALUNOS_TURMA
      ),
      QT_MAT_ATU_IMPUTADO = dplyr::if_else(
        ATU_IMPUTADO,
        QT_MAT_TOTAL,
        0
      ),
      QT_TURMAS_ESTIMADAS = dplyr::if_else(
        QT_MAT_TOTAL > 0,
        QT_MAT_TOTAL / MEDIA_ALUNOS_TURMA_CALCULO,
        0
      )
    ) |>
    dplyr::group_by(
      ANO,
      SIGLA_UF,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO
    ) |>
    dplyr::summarise(
      QT_MAT_TOTAL = sum(QT_MAT_TOTAL),
      QT_TURMAS_ESTIMADAS = sum(QT_TURMAS_ESTIMADAS),
      QT_MAT_ATU_IMPUTADO = sum(QT_MAT_ATU_IMPUTADO),
      QT_MUNICIPIOS_ATU_IMPUTADO = dplyr::n_distinct(
        dplyr::if_else(ATU_IMPUTADO, COD_MUN, NA_character_),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(ANO, SIGLA_UF, NO_UF),
      GRUPO_LOCALIZACAO = c("Capital", "Interior"),
      ETAPA_ENSINO = etapas_esperadas,
      fill = list(
        QT_MAT_TOTAL = 0,
        QT_TURMAS_ESTIMADAS = 0,
        QT_MAT_ATU_IMPUTADO = 0,
        QT_MUNICIPIOS_ATU_IMPUTADO = 0
      )
    ) |>
    dplyr::mutate(
      ETAPA_ENSINO_NOME = dplyr::recode(
        ETAPA_ENSINO,
        "CRE" = "Creche",
        "PRE" = "Pre-escola",
        "AI" = "Anos iniciais",
        "AF" = "Anos finais",
        "EM" = "Ensino medio"
      ),
      MEDIA_ALUNOS_TURMA = dplyr::if_else(
        QT_TURMAS_ESTIMADAS > 0,
        QT_MAT_TOTAL / QT_TURMAS_ESTIMADAS,
        NA_real_
      )
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      match(GRUPO_LOCALIZACAO, c("Capital", "Interior")),
      match(ETAPA_ENSINO, etapas_esperadas)
    ) |>
    dplyr::select(
      ANO,
      SIGLA_UF,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      ETAPA_ENSINO_NOME,
      QT_MAT_TOTAL,
      QT_TURMAS_ESTIMADAS,
      QT_MAT_ATU_IMPUTADO,
      QT_MUNICIPIOS_ATU_IMPUTADO,
      MEDIA_ALUNOS_TURMA
    )

  row.names(resultado) <- NULL
  resultado
}
