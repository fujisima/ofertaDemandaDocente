#' Calcular projecao logistica de um indicador
#'
#' @param t0 Ano inicial da serie observada.
#' @param tf Ano final da serie observada.
#' @param lim Limite superior do indicador.
#' @param obs0 Valor observado do indicador no ano inicial.
#' @param obsf Valor observado do indicador no ano final.
#' @param t Ano ou vetor de anos a projetar. Os valores devem ser maiores que
#'   `tf`.
#'
#' @return Vetor numerico com os valores projetados para `t`.
#' @importFrom stats plogis
#' @export
projecao_logistica <- function(t0, tf, lim, obs0, obsf, t) {
  validar_projecao_logistica(t0 = t0, tf = tf, lim = lim, obs0 = obs0, obsf = obsf, t = t)

  calcular_projecao_logistica(t0 = t0, tf = tf, lim = lim, obs0 = obs0, obsf = obsf, t = t)
}

#' Calcular projecao logistica ate um target futuro
#'
#' @param t0 Ano inicial.
#' @param tf Ano final em que o target deve ser atingido.
#' @param lim Limite superior do indicador.
#' @param obs0 Valor observado do indicador no ano inicial.
#' @param target Valor esperado para o indicador no ano final.
#' @param t Ano ou vetor de anos da trajetoria. Por padrao, todos os anos entre
#'   `t0` e `tf`.
#'
#' @return Vetor numerico com os valores projetados para `t`.
#' @export
projecao_logistica_target <- function(t0, tf, lim, obs0, target, t = seq(t0, tf)) {
  validar_projecao_logistica_target(
    t0 = t0,
    tf = tf,
    lim = lim,
    obs0 = obs0,
    target = target,
    t = t
  )

  calcular_projecao_logistica(t0 = t0, tf = tf, lim = lim, obs0 = obs0, obsf = target, t = t)
}

calcular_projecao_logistica <- function(t0, tf, lim, obs0, obsf, t) {
  transf0 <- log(obs0 / (lim - obs0))
  transff <- log(obsf / (lim - obsf))
  variacao <- (transff - transf0) / (tf - t0)
  auxt <- transff + variacao * (t - tf)

  plogis(auxt) * lim
}

validar_entradas_numericas <- function(...) {
  entradas <- list(...)

  for (nome in names(entradas)) {
    valor <- entradas[[nome]]

    if (!is.numeric(valor) || anyNA(valor) || any(!is.finite(valor))) {
      stop(sprintf("`%s` deve ser numerico, finito e sem valores ausentes.", nome), call. = FALSE)
    }
  }

  invisible(TRUE)
}

validar_projecao_logistica <- function(t0, tf, lim, obs0, obsf, t) {
  validar_entradas_numericas(t0 = t0, tf = tf, lim = lim, obs0 = obs0, obsf = obsf, t = t)

  if (length(t0) != 1 || length(tf) != 1 || length(lim) != 1) {
    stop("`t0`, `tf` e `lim` devem ter comprimento 1.", call. = FALSE)
  }

  if (tf == t0) {
    stop("`tf` deve ser diferente de `t0`.", call. = FALSE)
  }

  if (lim <= 0) {
    stop("`lim` deve ser maior que zero.", call. = FALSE)
  }

  if (any(obs0 <= 0 | obs0 >= lim)) {
    stop("`obs0` deve ser maior que zero e menor que `lim`.", call. = FALSE)
  }

  if (any(obsf <= 0 | obsf >= lim)) {
    stop("`obsf` deve ser maior que zero e menor que `lim`.", call. = FALSE)
  }

  if (any(t <= tf)) {
    stop("`t` deve conter apenas anos maiores que `tf`.", call. = FALSE)
  }

  invisible(TRUE)
}

validar_projecao_logistica_target <- function(t0, tf, lim, obs0, target, t) {
  validar_entradas_numericas(t0 = t0, tf = tf, lim = lim, obs0 = obs0, target = target, t = t)

  if (length(t0) != 1 || length(tf) != 1 || length(lim) != 1 || length(obs0) != 1 || length(target) != 1) {
    stop("`t0`, `tf`, `lim`, `obs0` e `target` devem ter comprimento 1.", call. = FALSE)
  }

  if (tf <= t0) {
    stop("`tf` deve ser maior que `t0`.", call. = FALSE)
  }

  if (lim <= 0) {
    stop("`lim` deve ser maior que zero.", call. = FALSE)
  }

  if (obs0 <= 0 || obs0 >= lim) {
    stop("`obs0` deve ser maior que zero e menor que `lim`.", call. = FALSE)
  }

  if (target <= 0 || target >= lim) {
    stop("`target` deve ser maior que zero e menor que `lim`.", call. = FALSE)
  }

  if (any(t < t0 | t > tf)) {
    stop("`t` deve conter apenas anos entre `t0` e `tf`.", call. = FALSE)
  }

  invisible(TRUE)
}
