
test_that("sql_lib basic test", {
    db.name <- paste("mtcars.db", sep = "")
    sql <- rsql(drv = RSQLite::SQLite(), dbname = db.name)
    query.sql <- sql$gen_select(select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt",
        table = "qsec", "vs", "am", where_fields="gear", where_values="carb"), "mtcars")
    mtcars.df <- sql$execute_select(query.sql)
    sql$disconnect()
    expect_equal(nrow(mtcars.df), 32)
})

test_that("sql_lib insert and delete test", {
    db.name <- paste("mtcars.db", sep = "")
    sql <- rsql(drv = RSQLite::SQLite(), dbname = db.name)
    insert.fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
        "gear", "carb")
    insert.data <- data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    insert.sql <- sql$gen_insert("mtcars", insert.fields, insert.data)
    sql$execute_insert(insert.sql)

    update.fields <- c("mpg", "cyl")
    update.values <- data.frame(1, 2)

    where.fields <- "disp"
    where.values <- 258.0
    update.sql <- sql$gen_update("mtcars", update.fields, update.values, where_fields, where_values)
    sql$execute_update(update.sql)

    delete.sql <- sql$gen_delete("mtcars", c("mpg"), c("1"))
    sql$execute_delete(delete.sql)

    sql$disconnect()

})

test_that("sql_lib select with where clause", {
    db.name <- paste("mtcars.db", sep = "")
    sql <- rsql(drv = RSQLite::SQLite(), dbname = db.name)
    query.sql <- sql$generate_select(table = "mtcars", select_fields = "*", where_fields = c("mpg"),
        where_values = c("1"))
    selected.mtcars = sql$execute_select(query.sql)
    sql$disconnect()
})
