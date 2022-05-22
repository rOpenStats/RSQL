context("util")
test_that("sql_lib basic test", {
  #Cannot getLogger from a vector. Should be an R6 object
  r6.dummy <- R6Class()$new()
  expect_error(getLogger(r6.dummy))

  rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = getMtcarsdbPath())
  rsql$logger <- NA
  class(rsql$logger)[[1]]
  #Cannot getLogger from an R6 object without an initialized log
  expect_error(getLogger(rsql))
})
