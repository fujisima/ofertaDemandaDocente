ler_pnadc_localizacao <- function(ano, trimestre) {
  PNADcIBGE::get_pnadc(
    year = ano,
    quarter = trimestre,
    design = FALSE,
    labels = FALSE,
    vars = c(
      "UF", "RM_RIDE", "Capital", "V20081", "V20082", "V2009",
      "V3002", "V3003A", "V3008", "V3009A", "V3014", "VD3004"
    )
  )
}

calcular_populacao_localizacao <- function(pnadcibge, ano, trimestre) {
  pnadcibge_tratada <- pnadcibge |>
    dplyr::group_by(UF) |>
    dplyr::mutate(
      mes_nascimento = suppressWarnings(as.numeric(as.character(V20081))),
      ano_nascimento = suppressWarnings(as.numeric(as.character(V20082))),
      idade_informada = suppressWarnings(as.numeric(as.character(V2009))),
      idade_cnei_raw = dplyr::if_else(
        is.na(mes_nascimento) |
          is.na(ano_nascimento) |
          mes_nascimento == 99 |
          ano_nascimento == 9999,
        idade_informada,
        ano - ano_nascimento - dplyr::if_else(mes_nascimento > 3, 1, 0)
      ),
      # Criança com idade negativa fica com zero ano.
      idade_cnei = pmax(idade_cnei_raw, 0),
      faixa_etaria = dplyr::case_when(
        dplyr::between(idade_cnei, 0, 3) ~ "0 a 3 anos",
        dplyr::between(idade_cnei, 4, 5) ~ "4 a 5 anos",
        dplyr::between(idade_cnei, 6, 10) ~ "6 a 10 anos",
        dplyr::between(idade_cnei, 11, 14) ~ "11 a 14 anos",
        dplyr::between(idade_cnei, 15, 17) ~ "15 a 17 anos",
        dplyr::between(idade_cnei, 18, 19) ~ "18 a 19 anos",
        TRUE ~ NA_character_
      ),
      # Nas UFs com RM/RIDE identificada, o polo urbano corresponde à RM/RIDE.
      # Nas demais UFs, a capital é usada como recorte alternativo.
      criterio_localizacao = dplyr::if_else(
        any(!is.na(RM_RIDE)),
        "RM/RIDE",
        "Capital"
      ),
      grupo_localizacao = dplyr::case_when(
        criterio_localizacao == "RM/RIDE" & !is.na(RM_RIDE) ~ "Polo urbano",
        criterio_localizacao == "Capital" & !is.na(Capital) ~ "Polo urbano",
        TRUE ~ "Demais áreas"
      ),
      pessoa = 1
    ) |>
    dplyr::ungroup()

  desenho_pnadc <- PNADcIBGE::pnadc_design(
    data_pnadc = tibble::as_tibble(pnadcibge_tratada)
  )

  # A restrição da população é feita depois da criação do desenho para
  # preservar sua estrutura na estimação das variâncias.
  desenho_faixas_escolares <- subset(
    desenho_pnadc,
    !is.na(faixa_etaria)
  )

  # Totais populacionais ponderados pelo desenho amostral da PNAD Contínua.
  survey::svyby(
    ~pessoa,
    ~UF + criterio_localizacao + faixa_etaria + grupo_localizacao,
    design = desenho_faixas_escolares,
    FUN = survey::svytotal,
    na.rm = TRUE,
    vartype = "se"
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      populacao_estimada = pessoa,
      erro_padrao = se
    ) |>
    dplyr::mutate(
      coeficiente_variacao = dplyr::if_else(
        populacao_estimada > 0,
        erro_padrao / populacao_estimada,
        NA_real_
      )
    ) |>
    tidyr::complete(
      tidyr::nesting(UF, criterio_localizacao),
      faixa_etaria,
      grupo_localizacao = c("Polo urbano", "Demais áreas"),
      fill = list(
        populacao_estimada = 0,
        erro_padrao = 0,
        coeficiente_variacao = NA_real_
      )
    ) |>
    dplyr::group_by(UF, faixa_etaria) |>
    dplyr::mutate(
      populacao_uf_faixa = sum(populacao_estimada),
      percentual_uf_faixa = dplyr::if_else(
        populacao_uf_faixa > 0,
        100 * populacao_estimada / populacao_uf_faixa,
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ano_referencia = ano,
      trimestre_referencia = trimestre
    ) |>
    dplyr::arrange(
      ano_referencia,
      trimestre_referencia,
      UF,
      faixa_etaria,
      match(grupo_localizacao, c("Polo urbano", "Demais áreas"))
    )
}
