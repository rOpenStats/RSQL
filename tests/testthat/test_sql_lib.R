# Basic functionality test
test_that("sql_lib basic test", {
  db.name <- paste("mtcars.db", sep="")
  sql <- rsql(drv=RSQLite::SQLite(), dbname = db.name)
  query.sql <- sql$generate_select(c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"),
                                   "mtcars")
  mtcars.df <- sql$execute_select(query.sql)
  sql$disconnect()
  expect_equal(nrow(mtcars.df), 32)
})

test_that("sql_lib insert and delete test", {
  db.name <- paste("mtcars.db", sep="")
  sql <- rsql(drv=RSQLite::SQLite(), dbname = db.name)
  insert.fields <- c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb")
  insert.data <- data.frame(1,2,3,4,5,6,7,8,9,10,11)
  insert.sql <- sql$generate_insert("mtcars",insert.fields,
                                    insert.data)
  sql$execute_insert(insert.sql)
  delete.sql <- sql$generate_delete("mtcars", c("mpg"), c("1"))
  sql$execute_delete(delete.sql)

  sql$disconnect()

})

test_that("sql_lib select with where clause", {
  db.name <- paste("mtcars.db", sep="")
  sql <- rsql(drv=RSQLite::SQLite(), dbname = db.name)
  query.sql <- sql$generate_select(table="mtcars",
                                   select_fields="*",
                                   where_fields =  c("mpg"),
                                   where_values = c("1"))
  selected.mtcars = sql$execute_select(query.sql)
  sql$disconnect()
})
