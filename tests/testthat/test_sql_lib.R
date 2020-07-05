
db.name <- getMtcarsdbPath()
rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)

test_that("sql_lib basic test", {

    #dbWriteTable(rsql$conn, name = "mtcars", mtcars, overwrite = TRUE)
    expect_equal(dbListTables(rsql$conn), "mtcars")

    db.name <- getMtcarsdbPath()
    rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
    query_sql <- rsql$gen_select(
        select_fields = c("*"),
        table = "mtcars")

#    query_sql <- rsql$gen_select(select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt",
#                                                   -        table = "qsec", "vs", "am", where_fields = "gear", where_values = "carb"), "mtcars")

    query_sql <- rsql$gen_select(
        select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
        table = "mtcars",
        where_fields = "gear",
        where_values = 4)


    # As vector casted to data.frames makes rows, next statement throws an error
    expect_error(rsql$gen_select(
        select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
        table = "mtcars",
        where_fields = c("gear", "carb"),
        where_values = c(4, 1)))

    query_sql <- rsql$gen_select(
        select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
        table = "mtcars",
        where_values = data.frame(gear = 4))
    mtcars.observed <- rsql$execute_select(query_sql)
    expect_equal(nrow(mtcars.observed), 12)
})

test_that("util", {
    expect_equal(rm_vector_quotes(c("hel'lo", "'hel'lo'", "'tell''"))
                 , c("hel''lo", "hel''lo", "tell''"))
})

test_that("legal entities", {

    #dbWriteTable(rsql$conn, name = "mtcars", mtcars, overwrite = TRUE)
    query_sql <- rsql$gen_select(select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt"),
                                                   table = "qsec")
    expect_equal(query_sql, "select mpg, cyl, disp, hp, drat, wt from qsec")

    expect_error(query_sql <- rsql$gen_select(select_fields = c("ill.egal1"),
                                 table = "legal"))

    expect_error(query_sql <- rsql$gen_select(select_fields = c("ill egal2"),
                                              table = "legal"))

    expect_error(query_sql <- rsql$gen_select(select_fields = c("legal"),
                                              table = "illegal 3"))
    #aggregation functions
    query_sql <- rsql$gen_select(select_fields = c("min(legal)"),
                                              table = "legal")
    expect_equal(query_sql,  "select min(legal) from legal")
    query_sql <- rsql$gen_select(select_fields = c("min(d_date)"),
                                 table = "legal")
    expect_equal(query_sql,  "select min(d_date) from legal")
    expect_error(query_sql <- rsql$gen_select(select_fields = c("illegal(legal)"),
                                 table = "legal"))
})

test_that("sql_lib insert and delete test", {
    insert_fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
        "gear", "carb")
    insert_data <- data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    insert.sql <- rsql$gen_insert(table = "mtcars", values_df = insert_data, insert_fields = insert_fields)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl")
    update_values <- data.frame(1, 2)

    where_values <- data.frame(disp = 258.0)
    update_sql <- rsql$gen_update(table = "mtcars",
                                  update_fields = update_fields, values = update_values,
                                  where_values = where_values)
    rsql$execute_update(update_sql)

    delete.sql <- rsql$gen_delete("mtcars", where_values = data.frame(mpg = 1))
    rsql$execute_delete(delete.sql)
})


test_that("sql_lib select, insert and delete with dataframe", {
    insert_fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
                       "gear", "carb")
    insert_data <- data.frame(matrix(1: ( 11 * 7 ), ncol = 11, byrow = TRUE))
    names(insert_data) <- insert_fields
    insert.sql <- rsql$gen_insert("mtcars", values_df = insert_data)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl")
    update_values <- data.frame(1, 2)
    names(update_values) <- update_fields
    where_values <- data.frame(disp = 275.8)
    update_sql <- rsql$gen_update("mtcars", values = update_values,
                                  where_values = where_values)
    rsql$execute_update(update_sql)

    check_sql <- rsql$gen_select(update_fields,
                                 table = "mtcars",
                                 where_values = where_values,
                                 distinct = TRUE)
    check_df <- rsql$execute_select(check_sql)
    expect_equivalent(check_df %>% dplyr::group_by(mpg, cyl), update_values)

    delete.sql <- rsql$gen_delete("mtcars", where_values = data.frame(mpg = 1))
    rsql$execute_delete(delete.sql)

})


test_that("sql_lib select, insert, update and delete with dataframe and stuffed apostrophe", {
    mtcars$model <- rownames(mtcars)
    dbWriteTable(rsql$conn, name = "mtcars", value = mtcars, overwrite = TRUE)
    insert_fields <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
                       "gear", "carb")
    insert_data <- data.frame(matrix(1: ( 11 * 7 ), ncol = 11, byrow = TRUE))
    names(insert_data) <- insert_fields
    insert_data$model <- paste("'car", 1:7, "'", sep = "")
    insert_data[7, "model"] <- "'O'Connor'"
    insert.sql <- rsql$gen_insert("mtcars", values_df = insert_data)
    rsql$execute_insert(insert.sql)

    update_fields <- c("mpg", "cyl", "model")
    update_values <- data.frame(1, 2, "'O'Connor'")
    names(update_values) <- update_fields
    where_values_df <- data.frame(disp = 275.8)
    update_sql <- rsql$gen_update("mtcars", values = update_values,
                                  where_values = where_values_df)
    rsql$execute_update(update_sql)

    where.accent <- "áccent"
    #where.accent <- "Bélgica"
    where.df <- data.frame(model = where.accent, stringsAsFactors = FALSE)
    exists.sql <- rsql$gen_select("id_pais", table = "pais",
                                  where_values = where.df)
    exists.sql


    where_values.df <- data.frame(model = "'O'Connor'")

    check_sql <- rsql$gen_select(update_fields,
                                 table = "mtcars",
                                 where_values = where_values.df,
                                 distinct = TRUE)
    expect_equal(check_sql, "select distinct mpg, cyl, model from mtcars where (model) in ('O''Connor')")
    check_df <- rsql$execute_select(check_sql)
    observed_values <- check_df %>% dplyr::filter(mpg == 1)
    observed_values$model <- add_quotes(observed_values$model)
    expect_equivalent(observed_values, update_values)

    # With or without quotes
    delete.where.df <- data.frame(model = "O'Connor")
    delete.sql <- rsql$gen_delete("mtcars", where_values = delete.where.df)
    expect_equal(delete.sql, "delete from mtcars where (model) in ('O''Connor')")
    delete.where.df <- data.frame(model = "'O'Connor'")
    #delete.where.df <- data.frame(model = "'O'Connor'")
    delete.sql <- rsql$gen_delete("mtcars", where_values = delete.where.df)
    expect_equal(delete.sql, "delete from mtcars where (model) in ('O''Connor')")
    rsql$execute_delete(delete.sql)
    check_df <- rsql$execute_select(check_sql)
    expect_equal(nrow(check_df), 0)

})

test_that("sql_lib select with where clause", {
    query_sql <- rsql$gen_select(table = "mtcars", select_fields = "*",
                                where_values = data.frame(mpg = 21))
    selected.mtcars <- rsql$execute_select(query_sql)
    expect_equal(selected.mtcars$drat, c(3.9, 3.9))
})


test_that("retrieveInsert", {
    retrieve.insert.df <- data.frame(vehicle_id = 0, uk = "car",
                                     color = "red", stringsAsFactors = FALSE)
    dbWriteTable(rsql$conn, name = "retrieveInsert", retrieve.insert.df, overwrite = TRUE)

    values.uk <- data.frame(uk = retrieve.insert.df[, "uk"], stringsAsFactors = FALSE)
    values.color <- data.frame(color = retrieve.insert.df[, "color"], stringsAsFactors = FALSE)

    vehicle.id.observed <-
        rsql$retrieve_insert(table = "retrieveInsert", values_uk = values.uk,
                         values = values.color, field_id = "vehicle_id")
    expect_equal(vehicle.id.observed, 0)

    values.uk <- data.frame(uk = "truck", stringsAsFactors = FALSE)

    vehicle.id.observed <-
        rsql$retrieve_insert(table = "retrieveInsert", values_uk = values.uk,
                             values = values.color, field_id = "vehicle_id")

    expect_true(is.na(vehicle.id.observed))#
    expect_equal(2, nrow(rsql$execute_select(rsql$gen_select("*", "retrieveInsert"))))

    # After insert, only execute retrieve
    vehicle.id.observed <-
        rsql$retrieve(table = "retrieveInsert", values_uk = values.uk,
                      field_id = "vehicle_id")
    expect_true(is.na(vehicle.id.observed))#

    vehicle.id.observed <-
        rsql$retrieve(table = "retrieveInsert", values_uk = values.uk,
                      field_id = "vehicle_id")
    expect_true(is.na(vehicle.id.observed))#


    vehicle_id <- data.frame(vehicle_id = 1)
    #Update vehicle_id to 1
    rsql$execute_update(rsql$gen_update("retrieveInsert", values = vehicle_id, where_values = values.uk))
    vehicle.id.observed <-
        rsql$retrieve_insert(table = "retrieveInsert", values_uk = values.uk,
                             values = values.color, field_id = "vehicle_id")
    expect_equal(1, vehicle.id.observed)
})

test_that("update symbols", {
    update.pk <- data.frame(pk = 1, stringsAsFactors = FALSE)
    update.values <- data.frame(field.1 = "a", field.2 = "b", field.3 = NA, stringsAsFactors = FALSE)
    observed.update <- sql_gen_update("foo", values = update.values, where_values = update.pk)
    expect_equal(observed.update, "update foo set (field.1,field.2,field.3)=('a','b',NULL) where (pk) in ('1')")
})

rsql$disconnect()

# TODO group by
