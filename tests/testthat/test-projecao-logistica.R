test_that("projecao_logistica segue protocolo da transformacao logistica", {
  t0 <- 2020
  tf <- 2025
  lim <- 100
  obs0 <- 40
  obsf <- 70
  t <- c(2026, 2030)

  transf0 <- log(obs0 / (lim - obs0))
  transff <- log(obsf / (lim - obsf))
  variacao <- (transff - transf0) / (tf - t0)
  auxt <- transff + variacao * (t - tf)
  esperado <- (exp(auxt) / (1 + exp(auxt))) * lim

  resultado <- projecao_logistica(
    t0 = t0,
    tf = tf,
    lim = lim,
    obs0 = obs0,
    obsf = obsf,
    t = t
  )

  expect_equal(resultado, esperado, tolerance = 1e-10)
})

test_that("projecao_logistica respeita limite do indicador", {
  resultado <- projecao_logistica(
    t0 = 2020,
    tf = 2025,
    lim = 100,
    obs0 = 40,
    obsf = 70,
    t = 2026:2035
  )

  expect_true(all(resultado > 0))
  expect_true(all(resultado < 100))
  expect_true(is.unsorted(resultado, strictly = TRUE) == FALSE)
})

test_that("projecao_logistica valida parametros", {
  expect_error(
    projecao_logistica(t0 = 2020, tf = 2020, lim = 100, obs0 = 40, obsf = 70, t = 2025),
    "`tf` deve ser diferente de `t0`",
    fixed = TRUE
  )

  expect_error(
    projecao_logistica(t0 = 2020, tf = 2025, lim = 100, obs0 = 100, obsf = 70, t = 2025),
    "`obs0` deve ser maior que zero e menor que `lim`",
    fixed = TRUE
  )

  expect_error(
    projecao_logistica(t0 = 2020, tf = 2025, lim = 100, obs0 = 40, obsf = 70, t = 2025),
    "`t` deve conter apenas anos maiores que `tf`",
    fixed = TRUE
  )
})

test_that("projecao_logistica_target atinge target no ano final", {
  anos <- 2025:2035

  resultado <- projecao_logistica_target(
    t0 = 2025,
    tf = 2035,
    lim = 100,
    obs0 = 50,
    target = 70,
    t = anos
  )

  expect_equal(resultado[anos == 2025], 50, tolerance = 1e-10)
  expect_equal(resultado[anos == 2035], 70, tolerance = 1e-10)
  expect_true(all(resultado > 0))
  expect_true(all(resultado < 100))
  expect_true(is.unsorted(resultado, strictly = TRUE) == FALSE)
})

test_that("projecao_logistica_target valida anos da trajetoria", {
  expect_error(
    projecao_logistica_target(t0 = 2025, tf = 2035, lim = 100, obs0 = 50, target = 70, t = 2036),
    "`t` deve conter apenas anos entre `t0` e `tf`",
    fixed = TRUE
  )
})
