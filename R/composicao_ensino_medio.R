ler_municipios_capital_rm <- function(
  path = "data-raw/Composicao_RM_2025_v2.xls"
) {
  if (!file.exists(path)) {
    stop(sprintf("Arquivo `%s` nao encontrado.", path), call. = FALSE)
  }

  composicao_rm <- readxl::read_excel(
    path,
    sheet = "Regiões Metropolitanas",
    col_types = "text"
  )

  validar_colunas(
    composicao_rm,
    c("SIGLA_UF", "COD_MUN", "NOME_MUN"),
    "composicao_rm"
  )

  capitais_ufs_sem_rm <- tibble::tibble(
    SIGLA_UF = c("AC", "DF", "MT", "MS", "PI"),
    COD_MUN = c("1200401", "5300108", "5103403", "5002704", "2211001"),
    NOME_MUN = c(
      "Rio Branco",
      "Brasília",
      "Cuiabá",
      "Campo Grande",
      "Teresina"
    ),
    CRITERIO_LOCALIZACAO = "Capital"
  )

  municipios_rm <- composicao_rm |>
    dplyr::select(SIGLA_UF, COD_MUN, NOME_MUN) |>
    dplyr::mutate(CRITERIO_LOCALIZACAO = "Região metropolitana")

  resultado <- dplyr::bind_rows(
    municipios_rm,
    capitais_ufs_sem_rm
  ) |>
    dplyr::mutate(
      COD_MUN = as.character(COD_MUN),
      GRUPO_LOCALIZACAO = "Capital"
    ) |>
    dplyr::distinct(SIGLA_UF, COD_MUN, .keep_all = TRUE) |>
    dplyr::arrange(SIGLA_UF, COD_MUN)

  codigos_duplicados <- resultado |>
    dplyr::count(COD_MUN, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(codigos_duplicados) > 0) {
    stop(
      "Ha codigos de municipio associados a mais de uma UF.",
      call. = FALSE
    )
  }

  resultado
}

ler_matriculas_em_municipio_2025 <- function(con = NULL) {
  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  resultado <- motherduck_read_sql(
    sql_path("matriculas_em_municipio_2025.sql"),
    con = con
  )

  validar_colunas(
    resultado,
    c(
      "ANO", "NO_UF", "COD_MUN", "QT_MAT_EM_TOTAL",
      "QT_MAT_EM_PROPEDEUTICO", "QT_MAT_EM_EPT"
    ),
    "matriculas_em_municipio_2025"
  )

  resultado
}

calcular_percentuais_em_localizacao <- function(
  matriculas_em_municipio,
  municipios_capital_rm
) {
  validar_colunas(
    matriculas_em_municipio,
    c(
      "ANO", "NO_UF", "COD_MUN",
      "QT_MAT_EM_PROPEDEUTICO", "QT_MAT_EM_EPT"
    ),
    "matriculas_em_municipio"
  )
  validar_colunas(
    municipios_capital_rm,
    c("COD_MUN"),
    "municipios_capital_rm"
  )

  codigos_capital_rm <- unique(as.character(municipios_capital_rm$COD_MUN))

  matriculas_em_municipio |>
    dplyr::mutate(
      COD_MUN = as.character(COD_MUN),
      GRUPO_LOCALIZACAO = dplyr::if_else(
        COD_MUN %in% codigos_capital_rm,
        "Capital",
        "Interior"
      )
    ) |>
    dplyr::group_by(ANO, NO_UF, GRUPO_LOCALIZACAO) |>
    dplyr::summarise(
      QT_MAT_EM_PROPEDEUTICO = sum(QT_MAT_EM_PROPEDEUTICO, na.rm = TRUE),
      QT_MAT_EM_EPT = sum(QT_MAT_EM_EPT, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(ANO, NO_UF),
      GRUPO_LOCALIZACAO = c("Capital", "Interior"),
      fill = list(
        QT_MAT_EM_PROPEDEUTICO = 0,
        QT_MAT_EM_EPT = 0
      )
    ) |>
    dplyr::mutate(
      QT_MAT_EM_TOTAL = QT_MAT_EM_PROPEDEUTICO + QT_MAT_EM_EPT,
      P_EM_PROPEDEUTICO = dplyr::if_else(
        QT_MAT_EM_TOTAL > 0,
        QT_MAT_EM_PROPEDEUTICO / QT_MAT_EM_TOTAL,
        NA_real_
      ),
      P_EM_EPT = dplyr::if_else(
        QT_MAT_EM_TOTAL > 0,
        QT_MAT_EM_EPT / QT_MAT_EM_TOTAL,
        NA_real_
      )
    ) |>
    dplyr::arrange(
      ANO,
      NO_UF,
      match(GRUPO_LOCALIZACAO, c("Capital", "Interior"))
    )
}
