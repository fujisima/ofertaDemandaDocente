ler_matriculas_turno_municipio_2025 <- function(con = NULL) {
  if (is.null(con)) {
    con <- motherduck_connect()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  motherduck_read_sql(
    sql_path("matriculas_turno_municipio_2025.sql"),
    con = con
  )
}

ler_matriculas_em_integral_2025 <- function(
  path = "data-raw/Demanda_038281_-_mat_integral_etapa.xlsx"
) {
  if (!file.exists(path)) {
    stop(sprintf("Arquivo `%s` nao encontrado.", path), call. = FALSE)
  }

  dados <- readxl::read_excel(
    path,
    sheet = "Planilha1",
    skip = 10,
    col_types = "text"
  )

  colunas_proped <- c(
    "QtMatEM1ano",
    "QtMatEM2ano",
    "QtMatEM3ano",
    "QtMatEM4ano",
    "QtMatEMNaoSeriada",
    "QtMatMag1",
    "QtMatMag2",
    "QtMatMag3",
    "QtMatMag4",
    "QtMatQPROF1",
    "QtMatQPROF2",
    "QtMatQPROF3",
    "QtMatQPROF4",
    "QtMatQP_NaoSeriada"
  )
  colunas_ept <- c(
    "QtMatTecInt1",
    "QtMatTecInt2",
    "QtMatTecInt3",
    "QtMatTecInt4",
    "QtMatTecIntNaoSeriada"
  )

  validar_colunas(
    dados,
    c(
      "NU_ANO_CENSO", "NO_UF", "CO_MUNICIPIO",
      colunas_proped, colunas_ept
    ),
    "matriculas_em_integral"
  )

  dados <- dados |>
    dplyr::filter(NU_ANO_CENSO == "2025")

  if (nrow(dados) == 0) {
    stop("Nao ha matriculas de ensino medio integral para 2025.", call. = FALSE)
  }

  converter_numericas <- function(dados, colunas) {
    resultado <- lapply(dados[colunas], function(coluna) {
      convertida <- suppressWarnings(as.numeric(coluna))
      if (any(!is.na(coluna) & is.na(convertida))) {
        stop(
          "Ha valores nao numericos nas colunas de matriculas integrais.",
          call. = FALSE
        )
      }
      convertida
    })
    as.data.frame(resultado, check.names = FALSE)
  }

  valores_proped <- converter_numericas(dados, colunas_proped)
  valores_ept <- converter_numericas(dados, colunas_ept)

  chaves <- tibble::tibble(
    ANO = as.integer(dados$NU_ANO_CENSO),
    NO_UF = as.character(dados$NO_UF),
    COD_MUN = as.character(dados$CO_MUNICIPIO)
  )

  dplyr::bind_rows(
    chaves |>
      dplyr::mutate(
        ETAPA_ENSINO = "EM",
        ETAPA_ENSINO_NOME = "Ensino medio",
        ETAPA_ENSINO_DETALHE = "EM_PROP",
        ETAPA_ENSINO_DETALHE_NOME = "Ensino medio propedeutico",
        QT_MAT_INTEGRAL = rowSums(valores_proped, na.rm = TRUE)
      ),
    chaves |>
      dplyr::mutate(
        ETAPA_ENSINO = "EM",
        ETAPA_ENSINO_NOME = "Ensino medio",
        ETAPA_ENSINO_DETALHE = "EM_EPT",
        ETAPA_ENSINO_DETALHE_NOME = "Ensino medio EPT",
        QT_MAT_INTEGRAL = rowSums(valores_ept, na.rm = TRUE)
      )
  ) |>
    dplyr::group_by(
      ANO,
      NO_UF,
      COD_MUN,
      ETAPA_ENSINO,
      ETAPA_ENSINO_NOME,
      ETAPA_ENSINO_DETALHE,
      ETAPA_ENSINO_DETALHE_NOME
    ) |>
    dplyr::summarise(
      QT_MAT_INTEGRAL = sum(QT_MAT_INTEGRAL),
      .groups = "drop"
    )
}

calcular_percentuais_matriculas_integral_localizacao <- function(
  matriculas_turno_municipio,
  matriculas_em_integral,
  totais_em_localizacao,
  municipios_capital_rm
) {
  colunas_identificacao <- c(
    "ANO", "NO_UF", "COD_MUN", "ETAPA_ENSINO",
    "ETAPA_ENSINO_NOME", "ETAPA_ENSINO_DETALHE",
    "ETAPA_ENSINO_DETALHE_NOME"
  )

  validar_colunas(
    matriculas_turno_municipio,
    c(colunas_identificacao, "QT_MAT_INTEGRAL", "QT_MAT_PARCIAL"),
    "matriculas_turno_municipio"
  )
  validar_colunas(
    matriculas_em_integral,
    c(colunas_identificacao, "QT_MAT_INTEGRAL"),
    "matriculas_em_integral"
  )
  validar_colunas(
    totais_em_localizacao,
    c(
      "ANO", "NO_UF", "GRUPO_LOCALIZACAO",
      "QT_MAT_EM_PROPEDEUTICO", "QT_MAT_EM_EPT"
    ),
    "totais_em_localizacao"
  )
  validar_colunas(
    municipios_capital_rm,
    "COD_MUN",
    "municipios_capital_rm"
  )

  codigos_capital_rm <- unique(as.character(municipios_capital_rm$COD_MUN))
  classificar_localizacao <- function(dados) {
    dados |>
      dplyr::mutate(
        COD_MUN = as.character(COD_MUN),
        GRUPO_LOCALIZACAO = dplyr::if_else(
          COD_MUN %in% codigos_capital_rm,
          "Capital",
          "Interior"
        )
      )
  }

  etapas_gerais <- classificar_localizacao(matriculas_turno_municipio) |>
    dplyr::group_by(
      ANO,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      ETAPA_ENSINO_NOME,
      ETAPA_ENSINO_DETALHE,
      ETAPA_ENSINO_DETALHE_NOME
    ) |>
    dplyr::summarise(
      QT_MAT_INTEGRAL = sum(QT_MAT_INTEGRAL, na.rm = TRUE),
      QT_MAT_PARCIAL = sum(QT_MAT_PARCIAL, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(
        ANO,
        NO_UF,
        ETAPA_ENSINO,
        ETAPA_ENSINO_NOME,
        ETAPA_ENSINO_DETALHE,
        ETAPA_ENSINO_DETALHE_NOME
      ),
      GRUPO_LOCALIZACAO = c("Capital", "Interior"),
      fill = list(QT_MAT_INTEGRAL = 0, QT_MAT_PARCIAL = 0)
    )

  integral_em <- classificar_localizacao(matriculas_em_integral) |>
    dplyr::group_by(
      ANO,
      NO_UF,
      GRUPO_LOCALIZACAO,
      ETAPA_ENSINO,
      ETAPA_ENSINO_NOME,
      ETAPA_ENSINO_DETALHE,
      ETAPA_ENSINO_DETALHE_NOME
    ) |>
    dplyr::summarise(
      QT_MAT_INTEGRAL = sum(QT_MAT_INTEGRAL, na.rm = TRUE),
      .groups = "drop"
    )

  totais_em <- dplyr::bind_rows(
    totais_em_localizacao |>
      dplyr::transmute(
        ANO,
        NO_UF,
        GRUPO_LOCALIZACAO,
        ETAPA_ENSINO = "EM",
        ETAPA_ENSINO_NOME = "Ensino medio",
        ETAPA_ENSINO_DETALHE = "EM_PROP",
        ETAPA_ENSINO_DETALHE_NOME = "Ensino medio propedeutico",
        QT_MAT_TOTAL = QT_MAT_EM_PROPEDEUTICO
      ),
    totais_em_localizacao |>
      dplyr::transmute(
        ANO,
        NO_UF,
        GRUPO_LOCALIZACAO,
        ETAPA_ENSINO = "EM",
        ETAPA_ENSINO_NOME = "Ensino medio",
        ETAPA_ENSINO_DETALHE = "EM_EPT",
        ETAPA_ENSINO_DETALHE_NOME = "Ensino medio EPT",
        QT_MAT_TOTAL = QT_MAT_EM_EPT
      )
  )

  chaves_em <- c(
    "ANO", "NO_UF", "GRUPO_LOCALIZACAO",
    "ETAPA_ENSINO", "ETAPA_ENSINO_NOME",
    "ETAPA_ENSINO_DETALHE", "ETAPA_ENSINO_DETALHE_NOME"
  )
  chaves_totais_em <- totais_em |>
    dplyr::group_by(dplyr::across(dplyr::all_of(chaves_em))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  if (any(chaves_totais_em$n != 1)) {
    stop(
      paste(
        "`totais_em_localizacao` deve ter uma linha por ano, UF, grupo",
        "e detalhamento do ensino medio."
      ),
      call. = FALSE
    )
  }

  integrais_sem_total <- integral_em |>
    dplyr::filter(QT_MAT_INTEGRAL > 0) |>
    dplyr::anti_join(totais_em, by = chaves_em)
  if (nrow(integrais_sem_total) > 0) {
    stop(
      "Ha matriculas integrais de ensino medio sem total correspondente.",
      call. = FALSE
    )
  }

  ensino_medio <- totais_em |>
    dplyr::left_join(
      integral_em,
      by = chaves_em,
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      QT_MAT_INTEGRAL = dplyr::coalesce(QT_MAT_INTEGRAL, 0),
      QT_MAT_PARCIAL = QT_MAT_TOTAL - QT_MAT_INTEGRAL
    )

  resultado <- etapas_gerais |>
    dplyr::mutate(
      QT_MAT_TOTAL = QT_MAT_INTEGRAL + QT_MAT_PARCIAL
    ) |>
    dplyr::bind_rows(ensino_medio)

  if (
    any(resultado$QT_MAT_INTEGRAL < 0, na.rm = TRUE) ||
      any(resultado$QT_MAT_PARCIAL < 0, na.rm = TRUE) ||
      any(resultado$QT_MAT_TOTAL < 0, na.rm = TRUE)
  ) {
    stop(
      paste(
        "As quantidades de matriculas devem ser nao negativas; verifique",
        "especialmente se o integral do ensino medio excede o total."
      ),
      call. = FALSE
    )
  }

  resultado |>
    dplyr::mutate(
      P_INTEGRAL = dplyr::if_else(
        QT_MAT_TOTAL > 0,
        QT_MAT_INTEGRAL / QT_MAT_TOTAL,
        NA_real_
      ),
      P_PARCIAL = dplyr::if_else(
        QT_MAT_TOTAL > 0,
        QT_MAT_PARCIAL / QT_MAT_TOTAL,
        NA_real_
      ),
      ordem_etapa = match(
        ETAPA_ENSINO,
        c("CRE", "PRE", "AI", "AF", "EM")
      ),
      ordem_detalhe = match(
        ETAPA_ENSINO_DETALHE,
        c("CRE", "PRE", "AI", "AF", "EM_PROP", "EM_EPT")
      ),
      ordem_grupo = match(
        GRUPO_LOCALIZACAO,
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
}
