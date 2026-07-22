test_that("consulta dados de acesso da educacao basica no MotherDuck", {
  skip_if_not(
    identical(Sys.getenv("RUN_MOTHERDUCK_TESTS"), "true"),
    "Teste MotherDuck desativado"
  )

  skip_if_not(
    nzchar(Sys.getenv("MOTHERDUCK_TOKEN")) || nzchar(Sys.getenv("motherduck_token")),
    "MOTHERDUCK_TOKEN nao configurado"
  )

  con <- ofertaDemandaDocente:::motherduck_connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  resultado <- ofertaDemandaDocente:::motherduck_read_sql(
    testthat::test_path("../../sql/teste.sql"),
    con = con
  )

  expect_named(resultado, c("ANO", "QTD"))
  expect_gt(nrow(resultado), 0)
  expect_true(all(!is.na(resultado$ANO)))
  expect_true(all(resultado$QTD > 0))
})
