#' Calcular taxa liquida de matricula
#'
#' @param matriculas_faixa_adequada Numero de matriculas na faixa etaria
#'   adequada.
#' @param populacao_faixa_adequada Populacao na faixa etaria adequada.
#'
#' @return Vetor numerico com a taxa liquida de matricula em percentual.
#' @export
taxa_liquida_matricula <- function(matriculas_faixa_adequada, populacao_faixa_adequada) {
  validar_razao_matricula(
    numerador = matriculas_faixa_adequada,
    denominador = populacao_faixa_adequada,
    nome_numerador = "matriculas_faixa_adequada",
    nome_denominador = "populacao_faixa_adequada"
  )

  matriculas_faixa_adequada / populacao_faixa_adequada * 100
}

#' Calcular taxa bruta de matricula
#'
#' @param matriculas_faixa_etaria Numero de matriculas na faixa etaria.
#' @param populacao_faixa_etaria Populacao na faixa etaria.
#'
#' @return Vetor numerico com a taxa bruta de matricula em percentual.
#' @export
taxa_bruta_matricula <- function(matriculas_faixa_etaria, populacao_faixa_etaria) {
  validar_razao_matricula(
    numerador = matriculas_faixa_etaria,
    denominador = populacao_faixa_etaria,
    nome_numerador = "matriculas_faixa_etaria",
    nome_denominador = "populacao_faixa_etaria"
  )

  matriculas_faixa_etaria / populacao_faixa_etaria * 100
}

#' Calcular percentual de matriculas fora da faixa etaria
#'
#' @param matriculas_fora_faixa Numero de matriculas fora da faixa etaria
#'   adequada.
#' @param total_matriculas Total de matriculas.
#'
#' @return Vetor numerico com o percentual de matriculas fora da faixa etaria.
#' @export
percentual_matriculas_fora_faixa <- function(matriculas_fora_faixa, total_matriculas) {
  validar_razao_matricula(
    numerador = matriculas_fora_faixa,
    denominador = total_matriculas,
    nome_numerador = "matriculas_fora_faixa",
    nome_denominador = "total_matriculas"
  )

  matriculas_fora_faixa / total_matriculas * 100
}

validar_razao_matricula <- function(numerador, denominador, nome_numerador, nome_denominador) {
  entradas <- list(numerador = numerador, denominador = denominador)
  nomes <- c(numerador = nome_numerador, denominador = nome_denominador)

  for (nome in names(entradas)) {
    valor <- entradas[[nome]]

    if (!is.numeric(valor) || anyNA(valor) || any(!is.finite(valor))) {
      stop(
        sprintf("`%s` deve ser numerico, finito e sem valores ausentes.", nomes[[nome]]),
        call. = FALSE
      )
    }
  }

  if (length(numerador) != length(denominador) && length(numerador) != 1 && length(denominador) != 1) {
    stop("Os vetores de entrada devem ter o mesmo comprimento ou comprimento 1.", call. = FALSE)
  }

  if (any(numerador < 0)) {
    stop(sprintf("`%s` deve ser maior ou igual a zero.", nome_numerador), call. = FALSE)
  }

  if (any(denominador <= 0)) {
    stop(sprintf("`%s` deve ser maior que zero.", nome_denominador), call. = FALSE)
  }

  invisible(TRUE)
}
