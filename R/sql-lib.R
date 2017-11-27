# Library for simple sql code generation from R You can learn more about package
# authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard
# shortcuts for package authoring: Build and Reload Package: 'Cmd + Shift + B'
# Check Package: 'Cmd + Shift + E' Test Package: 'Cmd + Shift + T' to
# internationalize gettext('no where_values specified', domain = 'R-rsql') does
# provide a tool to do it.


#' The class that provides the SQL functionality.
#'
#' This class is intended to simplify SQL commands.
#'
#' @importFrom R6 R6Class
#' @import RSQLite
#' @export
RSQL.class <- R6::R6Class("RSQL", public = list(connection = NA, driver = NA, db.name = NA,
    user = NA, password = NA, host = NA, port = NA, initialize = function(drv, dbname,
        user = NULL, password = NULL, host = NULL, port = NULL) {
        self$db.name <- dbname
        self$driver <- drv
        self$connection <- RSQLite::dbConnect(drv = self$driver, dbname = self$db.name)
    }, generate_select = function(select_fields, table, where_fields = NULL, where_values = NULL,
        group_by = c(), order_by = c(), top = 0) {
        sql_gen_select(select_fields, table, where_fields, where_values, group_by,
            order_by, top)
    }, generate_insert = function(table, insert_fields, values = c()) {
        sql_gen_insert(table, insert_fields, values)
    }, execute_update = function(sql_update, dbconn = NULL, export = c("db", "df")) {
        DMLcounter <- DMLcounter + 1
        sql_execute_update(sql_update, self$connection, export)
    }, execute_insert = function(sql_insert, export = c("db", "df")) {
        DMLcounter <- DMLcounter + 1
        sql_execute_insert(sql_insert, dbconn = self$connection, export = c("db",
            "df"))
    }, execute_select = function(sql_select) {
        DMLcounter <- DMLcounter + 1
        sql_execute_select(sql_select, dbconn = self$connection)
    }, generate_delete = function(table, where_fields = "", where_values = NULL) {
        sql_gen_delete(table, where_fields, where_values)
    }, execute_delete = function(sql_delete) {
        DMLcounter <- DMLcounter + 1
        sql_execute_delete(sql_delete, dbconn = self$connection)
    }, disconnect = function() {
        DBI::dbDisconnect(self$connection)
    }))

#' Produces a RSQL object
#'
#' @param drv Driver name
#' @param dbname Database name
#' @param user Database user name
#' @param password Database password
#' @param host Database host
#' @param port Database port
#' @export
rsql <- function(drv, dbname, user = NULL, password = NULL, host = NULL, port = NULL) {
    RSQL.class$new(drv, dbname, user, password, host, port)
}


#' Extecutes a statement on the database.
#'
#' @import futile.logger
#' @param sql_insert The SQL String
#' @param dbconn The Database Connection to run the query against
#' @param export The export type (either 'db' or 'df')
sql_execute_insert <- function(sql_insert, dbconn = NULL, export = c("db", "df")) {
    sql_insert <- gsub(",NA", ",NULL", sql_insert)
    sql_insert <- gsub(", NA", ",NULL", sql_insert)
    sql_insert <- paste(sql_insert, ";", sep = "")
    if ("db" %in% export) {
        ret <- DBI::dbSendQuery(dbconn, sql_insert)
        futile.logger::flog.info(sql_insert)

        # data <- fetch(rs,n=-1) print(res)
        if (length(ret) > 0) {
            # TODO check if OK

            # if (nchar(ret[1])>5){ print(paste('for processing', id_process, ' in Insert
            # No', contadorInserts, ' an error is inserted in database'))
            # print(paste(sql_insert)) print(paste('Error:',res[1]))
            # insertarErrorEnBD(id_process, contadorInserts, id_ciudad, anio, mes,
            # sql_insert, res[1],context) }
        }
    }
    if ("df" %in% export)
        sql_inserts <- rbind(sql_inserts, sql_insert)
    ret
}

#' Executes an update on the database
#'
#' @param sql_update The update SQL
#' @param dbconn The Database Connection to run the query against
#' @param export The export type (either 'db' or 'df')
sql_execute_update <- function(sql_update, dbconn = NULL, export = c("db", "df")) {
    sql_select <- gsub(",NA", ",NULL", sql_update)
    sql_select <- gsub(", NA", ",NULL", sql_update)
    ret <- DBI::dbGetQuery(dbconn, sql_update)
    ret
}


#' sql_execute_delete
#'
#' Executes a delete on the Database
#'
#' @param sql_delete The delete SQL
#' @param dbconn The Database Connection to run the query against
sql_execute_delete <- function(sql_delete, dbconn = NULL) {
    sql_delete <- gsub(",NA", ",NULL", sql_delete)
    sql_delete <- gsub(", NA", ",NULL", sql_delete)
    ret <- DBI::dbGetQuery(dbconn, sql_delete)
    futile.logger::flog.info(sql_delete)
    ret

}

#' Executes a select on the database
#'
#' @param sql_select The delete SQL
#' @param dbconn The Database Connection to run the query against
sql_execute_select <- function(sql_select, dbconn = NULL) {
    # debug sql_select <- 'select price from v_quotes_id_completed where
    # symbol='alua' order by date DESC LIMIT 1' sql_select <- 'select * from
    # v_quotes'
    sql_select <- gsub(",NA", ",NULL", sql_select)
    sql_select <- gsub(", NA", ",NULL", sql_select)
    ret <- DBI::dbGetQuery(dbconn, sql_select)
    ret
}

#' Executes the insert statement
#'
#' @param dbconn The db connection
#' @param sql_select The SQL select query
#' @param sql_insert The SQL insert query
#' @param ... other variables to considered.
execute_get_insert <- function(dbconn, sql_select, sql_insert, ...) {
    ret <- sql_execute_select(sql_select, dbconn)
    if (nrow(ret) == 0) {
        sql_execute_insert(sql_insert)
        ret <- sql_execute_select(sql_select, ...)
    }
    ret[1, ]
}

#' Determines if the string is quoted or not
#'
#' @param text The text to test
#' @param quotes_symbols The quotation characters
#' @export
is_quoted <- function(text, quotes_symbols = c("'", "'")) {
    ret <- FALSE
    i <- 1
    while (!ret & i <= length(quotes_symbols)) {
        quotes <- quotes_symbols[i]
        ret <- substr(text, 1, 1) == quotes & substr(text, nchar(text), nchar(text)) ==
            quotes
        i <- i + 1
    }
    ret
}

#' Removes the quotes from the string
#'
#' @param text The string to remove the quotes from.
dequote <- function(text) {
    substr(text, 2, nchar(text) - 1)
}

#' TODO: WHAT IS THIS FUNCTION DOING AGAIN?
#'
#' @param text The string
#' @param quotes The quotes
re_quote <- function(text, quotes = "'") {
    quote <- FALSE
    if (!is_quoted(text, "'"))
        quote <- TRUE
    if (is_quoted(text, "'")) {
        text <- dequote(text)
        quote <- TRUE
    }
    if (quote)
        text <- paste(quotes, text, quotes, sep = "")
    text
}

#' Adds quotes to a string
#'
#' @param text The string to quote
add_quotes <- function(text) {
    ret <- sapply(text, FUN = re_quote)
    names(ret) <- NULL
    ret
}

#' Removes quotes from the String
#'
#' @param text The string to remove quotes from
#' @param quotes Quote characters
rm_quotes <- function(text, quotes = "'") {
    if (quotes == substr(text, 1, 1) & quotes == substr(text, nchar(text), nchar(text))) {
        text <- substr(text, 2, nchar(text) - 1)
    }
    text
}

#' Removes quotes from data.frame colums
#'
#' @param text The text column to remove quotes from.
remove_quotes <- function(text) {
    ret <- sapply(text, FUN = rm_quotes)
    names(ret) <- NULL
    ret
}


#' add_grep_exact_match
#'
#' @param text TEST
add_grep_exact_match <- function(text) {
    text <- gsub("(\\^|\\%)", "\\\\\\1", text)
    paste("^", text, "$", sep = "")
}

#' Generates a Delete Statement
#'
#' @param table The table from which the delete statement will be generated
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
sql_gen_delete <- function(table, where_fields = "", where_values = NULL) {
    sql_where <- sql_gen_where(where_fields, where_values)
    ret <- paste("delete from", table, sql_where)
    ret

}

#' Generates a Select Statement
#'
#' @param select_fields The fields to be selected
#' @param table The table to be used in the select
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @param group_by Group by fields
#' @param order_by Order by fields
#' @param top Retrieve top records
sql_gen_select <- function(select_fields, table, where_fields = NULL, where_values = NULL,
    group_by = c(), order_by = c(), top = 0) {
    separator <- ""
    sql_select_fields <- ""
    for (f in select_fields) {
        sql_select_fields <- paste(sql_select_fields, separator, f, sep = "")
        separator <- ", "
    }
    sql_where <- sql_gen_where(where_fields, where_values)
    sql_order_by <- paste(order_by, collapse = ",")
    sql_group_by <- ""
    if (length(group_by) > 0) {
        separator <- ""
        for (f in group_by) {
            sql_group_by <- paste(sql_group_by, separator, f, sep = "")
            separator <- ", "
        }
        order_by <- c(sql_group_by, order_by)
        sql_group_by <- paste("group by ", sql_group_by)
    }
    ret <- paste("select", sql_select_fields, "from", table, sql_where, sql_group_by)
    if (nchar(sql_order_by) > 0)
        ret <- paste(ret, "order by", sql_order_by)
    if (top > 0)
        ret <- paste(ret, "limit", top)
    trimws(ret)
}

#' Generates a where statement to be used on a SQL statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
sql_gen_where <- function(where_fields, where_values) {
    ret <- ""
    if (!is.null(where_fields) & !is.null(where_values)) {
        # Asserts with values
        if (!is.vector(where_fields))
            stop(paste(gettext("sql_lib_where_files_has_to_be_a_vector", domain="R-rsql"), str(where_fields)))
        if (!is.list(where_values))
            if (is.vector(where_values))
                where_values <- data.frame(matrix(where_values, byrow = TRUE, ncol = length(where_values)),
                  stringsAsFactors = FALSE)
        if (length(where_fields) != ncol(where_values))
            stop(paste(gettext("sql_lib.where_fields_num_not_eq_where_values_num", domain="R-sql"), length(where_fields), "!=",
                length(where_values), paste(where_fields, collapse = ","), paste(where_values,
                  collapse = ",")))
        # if strings values, add '
        for (col in names(where_values)) {
            if (max(is.character(where_values[, col])) == 1) {
                # if there is at least one value character in column remove ' for normalization
                # and adding after
                new.values <- paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                  "\\1", where_values[, col]), "'", sep = "")
                futile.logger::flog.info(paste("col", col, "is character. Replacing values",
                  paste(where_values[, col], collapse = ","), "with values", paste(new.values,
                    collapse = ",")))
                where_values[, col] <- new.values
            }
        }


        ret <- ""
        if (length(where_fields) > 0 & !(length(where_fields) == 1 & nchar(where_fields[1]) ==
            0)) {
            where_values <- as.data.frame(where_values, nrow = nrow(where_values)/length(where_fields),
                stringsAsFactors = FALSE)
            if (nrow(where_values) > 2)
                ret <- sql_gen_where_list(where_fields, where_values) else ret <- sql_gen_where_or(where_fields, where_values)
        }
    } else {
        if (!is.null(where_fields)){
            stop(paste(gettext("sql_lib.no_where_values_specified", domain="R-rsql")))
        }
        if (!is.null(where_values)) {
          stop(paste(gettext("sql_lib.no_where_values_specified", domain="R-rsql")))
        }
    }
    ret
}

#' Generates a where list statement to be used on a SQL statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
sql_gen_where_list <- function(where_fields, where_values) {
    sql_where <- ""
    if (length(where_fields) > 0) {
        separator <- ""
        sql_where <- "where ("
        for (f in where_fields) {
            sql_where <- paste(sql_where, separator, f, sep = "")
            separator <- ","
        }
        sql_where <- paste(sql_where, ") in ", sep = "")
        list_where <- ""
        separator_list <- ""
        for (v in c(1:nrow(where_values))) {
            i <- 1
            separator <- ""
            sql_row_where <- ""
            for (f in where_fields) {
                if (is.na(where_values[v, i]))
                  value <- paste(":label_", f, ":", sep = "") else value <- where_values[v, i]

                if (is.character(value)) {
                  value <- add_quotes(value)
                }
                sql_row_where <- paste(sql_row_where, separator, value, sep = "")
                separator <- ","
                i <- i + 1
            }
            if (i > 2)
                sql_row_where <- paste("(", sql_row_where, ")", sep = "")
            list_where <- paste(list_where, separator_list, sql_row_where, sep = "")
            separator_list <- ","
        }
        sql_where <- paste(sql_where, "(", list_where, ")")
    }
    sql_where
}

#' Generates a where (or) statement to be used on a SQL statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
sql_gen_where_or <- function(where_fields, where_values) {
    sql_where <- ""
    if (length(where_fields) > 0) {
        sql_where <- "where"
        separator_where <- ""
        for (v in c(1:nrow(where_values))) {
            i <- 1
            separator <- ""
            sql_row_where <- ""
            for (f in where_fields) {
                if (is.na(where_values[v, i]))
                  value <- paste(":label_", f, ":", sep = "") else value <- where_values[v, i]
                if (is.character(value))
                  value <- add_quotes(value)
                sql_row_where <- paste(sql_row_where, separator, f, "=", value, sep = "")
                separator <- " and "
                i <- i + 1
            }
            sql_where <- paste(sql_where, separator_where, "(", sql_row_where, ")")
            separator_where <- " or "
        }
    }
    sql_where
}



#' Generates an insert statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @export
#'
#' @param table The table to be affected
#' @param insert_fields The fields to insert
#' @param values The values to insert
sql_gen_insert <- function(table, insert_fields, values = c()) {
    values <- as.data.frame(values, stringsAsFactors = FALSE)
    if (length(insert_fields) != ncol(values)) {
        stop(paste(gettext("sql_lib.incompatible_fields_and_data", domain="R-rsql"), length(insert_fields), gettext("sql_lib.not_eq", domain="R-rsql"),
            ncol(values), paste(insert_fields, collapse = ";"), paste(values, collapse = ";")))
    }
    separator <- ""
    sql_insert_fields <- ""
    for (f in insert_fields) {
        sql_insert_fields <- paste(sql_insert_fields, separator, f, sep = "")
        separator <- ", "
    }
    sql_values <- ""
    separator_rows <- ""
    for (i in c(1:nrow(values))) {
        sql_values_row <- ""
        separator <- ""
        for (j in c(1:length(insert_fields))) {
            if (is.na(values[i, j])) {
                value <- "NA"
            } else {
                value <- values[i, j]
                if (is.character(value)) {
                  value <- paste("'", value, "'", sep = "")
                }
            }
            sql_values_row <- paste(sql_values_row, separator, value, sep = "")
            separator <- ", "
        }
        futile.logger::flog.info(sql_values_row)
        sql_values <- paste(sql_values, separator_rows, "(", sql_values_row, ")")
        separator_rows <- ", "
    }
    ret <- paste("insert into ", table, "(", sql_insert_fields, ") values ", sql_values,
        sep = "")
    ret
}

#' Executes an update
#'
#' @param sql_insert The insert statement
#' @param dbconn the Database Connection
#' @param export The export method
sql_execute_update <- function(sql_insert, dbconn = NULL, export = c("db", "df")) {
    stop(gettext("sql_lib.not_implemented", domain="R-rsql"))
}

#' Generates an update statement
#'
#' @param table The table to update
#' @param update_fields The fields to update
#' @param values The values to update
#' @param fields_id The fields id
#' @param values_id The values id
sql_gen_update <- function(table, update_fields, values, fields_id, values_id) {
    stop(gettext("sql_lib.not_implemented", domain="R-rsql"))
    ret <- paste("update ", table, " set (", update_fields, ")=(", values,
        ") where ", sep = "")
    ret
}


#' Returns string w/o leading whitespace
#'
#' @param x The string
trim_leading <- function(x) sub("^\\s+", "", x)

#' Returns string w/o trailing whitespace
#'
#' @param x The string
trim_trailing <- function(x) sub("\\s+$", "", x)


#' Returns string w/o leading or trailing whitespace
#'
#' @param x The string
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#' renames a column on a data.frame
#'
#' @param df The date.frame
#' @param name The name of the column
#' @param replace_name The new name of the column
rename_col <- function(df, name, replace_name) {
    i <- which(names(df) == name)
    names(df)[i] <- replace_name
    df
}

#' TODO: WHAT DOES THIS DO AGAIN?
#'
#' @param ... The parameters
cbind_coerced <- function(...) {
    ret <- cbind(..., stringsAsFactors = FALSE)
    if ("stringsAsFactors" %in% names(ret))
        ret <- ret[, -which(names(ret) == "stringsAsFactors")]
    ret
}


#' Checks that the columns are in the data.frame
#'
#' @param dataframe The data.frame
#' @param columns The colums to check
df_verify <- function(dataframe, columns) {
    ret <- NULL
    dataframe_names <- names(dataframe)
    for (column in columns) {
        if (!column %in% dataframe_names)
            ret <- c(ret, column)
    }
    if (length(ret) > 0)
        stop(paste(gettext("sql_lib.missing_columns_in_dataframe", domain="R-rsql"), paste(ret, collapse = ","), "df",
            paste(dataframe_names, collapse = ",")))
}

#' Retrieves an insert Statement
#'
#' @param table The table
#' @param fields_id The fields ID
#' @param fields The fields
#' @param values The values
#' @param dbconn The database connection
sql_retrieve_insert <- function(table, fields_id, values_id, fields = NULL, values = NULL,
    dbconn = NULL) {
    ret <- NULL
    values_id <- as.data.frame(values_id, stringsAsFactors = FALSE)
    values <- as.data.frame(values, stringsAsFactors = FALSE)
    if (nrow(values) > 0 & nrow(values) < nrow(values_id)) {
        stop(paste(gettext("sql_lib.error_nrows_values_id_neq_nrows_values", domain="R-rsql"), nrow(values_id), nrow(values)))
    }


    for (i in c(1:nrow(values_id))) {
        # value_id <- as.character(values_id[i,]) value <- as.character(values[i,])
        value_id <- as.data.frame(values_id[i, ], stringsAsFactors = FALSE)
        value <- values[i, ]
        values_insert <- cbind_coerced(value_id, value)

        select_statement <- sql_gen_select("id", table, where_fields = fields_id,
            where_values = value_id)

        # debug
        futile.logger::flog.info(paste("verifying", select_statement, ":"))
        insert_statement <- sql_gen_insert(table, insert_fields = c(fields_id, fields),
            values = values_insert)
        row <- sql_execute_select(select_statement, dbconn = dbconn)
        futile.logger::flog.info(paste(row, "rows"))
        if (nrow(row) == 0) {
            futile.logger::flog.info(paste("executing", insert_statement))
            res <- sql_execute_insert(insert_statement, dbconn = dbconn)
            row <- sql_execute_select(select_statement, dbconn = dbconn)
        }
        ret <- c(ret, as.numeric(row$id))
        i <- i + 1
    }
    ret
}


#' Generates a Joined Query
#'
#' @param dw_definition TEST
#' @param recipe TEST
#' @param indicator_fields TEST
sql_gen_joined_query <- function(dw_definition, recipe, indicator_fields) {
    # sql_gen_select <- function(select_fields, table, where_fields='',
    # where_values=NULL,group_by=c()){
    sql_select_fields <- rep("", length(indicator_fields))
    sql_from <- ""
    sql_where <- ""
    ind_i <- 0
    where_sep <- ""
    from_sep <- ""
    for (i in c(1:nrow(recipe$m_recipe))) {
        current_expression <- recipe$m_recipe[i, ]
        # TODO correct in a dictionary
        alias <- gsub("\\.", "_", current_expression$value)
        for (j in c(1:length(indicator_fields))) {
            if (current_expression$op == "=")
                sql_select_fields[j] <- current_expression$value else {
                sql_select_fields[j] <- paste("(", sql_select_fields[j], current_expression$op,
                  alias, ".", indicator_fields[j], ")", sep = "")
            }
        }
        if (current_expression$value_type == "indicator") {
            ind_i <- ind_i + 1
            if (ind_i == 1)
                first_alias <- alias
            sql_from <- paste(sql_from, from_sep, dw_definition$m_fact_table, " ",
                alias, sep = "")
            from_sep <- ","
            first_field_def <- dw_definition$m_dimensions[1, ]
            for (k in c(1:nrow(dw_definition$m_dimensions))) {
                current_field_def <- dw_definition$m_dimensions[k, ]
                explicit_value <- ind_i == 1 & nchar(current_field_def$default) ==
                  0
                # TODO define in dw_definition fields mapping
                explicit_value <- explicit_value | current_field_def$field == "symbol"
                filter <- ind_i > 1 | current_field_def$field == "symbol"

                # debug explicit_value< <- explicit_value dw_definition< <- dw_definition k< <- k

                if (explicit_value) {
                  if (current_expression$value_id > 0)
                    right_value <- current_expression$value_id else right_value < paste("'", current_expression$value, "'", sep = "")
                } else {
                  if (filter)
                    right_value <- paste(first_alias, ".", current_field_def$field,
                      sep = "")
                }
                if (filter) {
                  sql_where <- paste(sql_where, where_sep, alias, ".", current_field_def$field,
                    "=", right_value, sep = "")
                  where_sep <- " and "
                }
            }
        }
        ret <- paste("select", paste(sql_select_fields, "as", indicator_fields, collapse = ","),
            "from", sql_from, "where", sql_where)
    }
    ret
}

#' Parses a where clause.
#'
#' @param where_clause_list The list of params
#' @import futile.logger
#' @export
parse_where_clause <- function(where_clause_list = c()) {
    where.df <- data.frame(lhs = character(), comp = character(), rhs = character(),
        stringsAsFactors = FALSE)
    names(where.df) <- c("lhs", "comp", "rhs")
    for (where_clause in where_clause_list) {
        where_struct = strsplit(where_clause, "!=")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], "!=", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }
        where_struct = strsplit(where_clause, "<=")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], "<=", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }
        where_struct = strsplit(where_clause, ">=")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], ">=", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }
        where_struct = strsplit(where_clause, "=")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], "=", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }
        where_struct = strsplit(where_clause, ">")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], ">", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }
        where_struct = strsplit(where_clause, "<")
        if (length(where_struct[[1]]) == 2) {
            where = data.frame(where_struct[[1]][1], "<", paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
                "\\1", where_struct[[1]][2]), "'", sep = ""))
            names(where) <- c("lhs", "comp", "rhs")
            where.df <- rbind(where.df, where)
            next
        }

    }
    where.df
}

#' Generates a where statement to be used on a SQL statement.
#'
#' @param where_clause_list The fields used in the where section
sql_gen_where.new <- function(where_clause_list) {
    ret <- ""
    if (!is.null(where_clause_list)) {
        # Asserts with values
        if (!is.vector(where_clause_list))
            stop(paste(gettext("sql_lib.where_clause_list_must_be_a_vector_but_is", domain="R-rsql"), str(where_clause_list)))
        if (!is.list(where_clause_list))
            if (is.vector(where_clause_list)) {
                where_clauses <- parse_where_clause(where_clause_list)
                #ret <- sql_gen_where_list(where_fields, where_values)
            }
    }
    ret
}

#' Generates a where list statement to be used on a SQL statement.
#'
#' @param where_clause.df The fields used in the where section
sql_gen_where_list.new <- function(where_clause.df) {
    sql_where <- ""
    separator <- ""
    for (where_clause in where_clause.df) {
        sql_where <- "where ("
        sql_where <- paste(sql_where, where_clause$lhs, where_clause$comp, where_clause$rhs,
            separator, sep = "")
        separator = " and "
        sql_where <- paste(sql_where, ") in ", sep = "")
    }
    sql_where
}


#' Operator IN for multiple columns
#'
#' @param x TEST
#' @param y TEST
"%IN%" <- function(x, y) interaction(x) %in% interaction(y)

