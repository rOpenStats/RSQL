# Basic functionality test
test_that("sql_lib basic test", {
  dbconn <- RSQLite::dbConnect(SQLite(), dbname = "../data/mtcars.db")
  query.sql <- sql_gen_select("*", "mtcars")
  mtcars.df <- dbGetQuery(dbconn, query.sql)
  expect_equal(nrow(mtcars.df), 32)
})
