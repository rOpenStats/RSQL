
test_that("sql_lib basic test", {

    db.name <- getCarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    #dbWriteTable(rsql$conn, name = "mtcars", mtcars, overwrite = TRUE)
    expect_equal(dbListTables(rsql$conn), "mtcars")

    query_sql <- rsql$gen_select(select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt",
        table = "qsec", "vs", "am", where_fields="gear", where_values="carb"), "mtcars")
    mtcars.observed <- rsql$execute_select(query_sql)
    rsql$disconnect()
    expect_equal(nrow(mtcars.observed), 31)
})

test_that("sql_lib insert and delete test", {
    db.name <- getCarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    insert.fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
        "gear", "carb")
    insert.data <- data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    names(insert.data) <- insert.fields
    insert.sql <- rsql$gen_insert("mtcars", values_df = insert.data, insert_fields = insert.fields)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl")
    update_values <- data.frame(1, 2)

    where_fields <- "disp"
    where_values <- 258.0
    update_sql <- rsql$gen_update("mtcars", update_fields, update_values, where_fields, where_values)
    rsql$execute_update(update_sql)

    delete.sql <- rsql$gen_delete("mtcars", c("mpg"), c("1"))
    rsql$execute_delete(delete.sql)

    rsql$disconnect()

})

test_that("sql_lib select with where clause", {
    db.name <- getCarsdbPath()
    sql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    query_sql <- sql$gen_select(table = "mtcars", select_fields = "*", where_fields = c("mpg"),
        where_values = c("1"))
    selected.mtcars = sql$execute_select(query_sql)
    sql$disconnect()
})
