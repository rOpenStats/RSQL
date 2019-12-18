
test_that("sql_lib basic test", {

    db.name <- getMtcarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    #dbWriteTable(rsql$conn, name = "mtcars", mtcars, overwrite = TRUE)
    expect_equal(dbListTables(rsql$conn), "mtcars")

    query_sql <- rsql$gen_select(select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt",
        table = "qsec", "vs", "am", where_fields="gear", where_values ="carb"), "mtcars")
    mtcars.observed <- rsql$execute_select(query_sql)
    expect_equal(nrow(mtcars.observed), 31)
    rsql$disconnect()
})

test_that("sql_lib insert and delete test", {
    db.name <- getMtcarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    insert_fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
        "gear", "carb")
    insert_data <- data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    insert.sql <- rsql$gen_insert("mtcars", values_df = insert_data, insert_fields = insert_fields)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl")
    update_values <- data.frame(1, 2)

    where_fields <- "disp"
    where_values <- 258.0
    update_sql <- rsql$gen_update(table = "mtcars",
                                  update_fields = update_fields, values = update_values,
                                  where_fields = where_fields, where_values = where_values)
    rsql$execute_update(update_sql)

    delete.sql <- rsql$gen_delete("mtcars", c("mpg"), c("1"))
    rsql$execute_delete(delete.sql)

    rsql$disconnect()
})


test_that("sql_lib select, insert and delete with dataframe", {
    db.name <- getMtcarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    insert_fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
                       "gear", "carb")
    insert_data <- data.frame(matrix(1:(11*7), ncol = 11, byrow = TRUE))
    names(insert_data) <- insert_fields
    insert.sql <- rsql$gen_insert("mtcars", values_df = insert_data)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl")
    update_values <- data.frame(1, 2)
    names(update_values) <- update_fields
    where_fields <- "disp"
    where_values <- 275.8
    names(where_values) <- where_fields
    update_sql <- rsql$gen_update("mtcars", values = update_values,
                                  where_values = where_values)
    rsql$execute_update(update_sql)

    check_sql <- rsql$gen_select(update_fields,
                                 table = "mtcars",
                                 where_values = where_values,
                                 distinct = TRUE)
    check_df <- rsql$execute_select(check_sql)
    expect_equal(check_df %>% group_by(mpg, cyl), update_values)

    delete.sql <- rsql$gen_delete("mtcars", c("mpg"), c("1"))
    rsql$execute_delete(delete.sql)

    rsql$disconnect()
})

test_that("sql_lib select with where clause", {
    db.name <- getMtcarsdbPath()
    sql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    query_sql <- sql$gen_select(table = "mtcars", select_fields = "*",
                                where_fields = c("mpg"),
                                where_values = 21)
    selected.mtcars = sql$execute_select(query_sql)
    expect_equal(selected.mtcars$drat, c(3.9, 3.9))
    sql$disconnect()
})

# TODO group by
