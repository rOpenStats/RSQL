# Basic functionality test
test_that("sql_lib basic test", {
  sql <- rsql(drv=RSQLite::SQLite(), dbname = paste(getwd(), "/tests/data/mtcars.db", sep=""))
  query.sql <- sql$generate_select(c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"),
                                   "mtcars")
  mtcars.df <- sql$execute_select(query.sql)
  sql$disconnect()
  expect_equal(nrow(mtcars.df), 32)
})

test_that("sql_lib ", {

})
