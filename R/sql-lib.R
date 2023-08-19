# Library for simple sql code generation from R You can learn more about package
# authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard
# shortcuts for package authoring: Build and Reload Package: 'Cmd + Shift + B'
# Check Package: 'Cmd + Shift + C' Test Package: 'Cmd + Shift + T' to
# internationalize gettext('no where_values specified', domain = 'R-rsql') does
# provide a tool to do it.



#' The class that provides the SQL functionality.
#'
#' @description
#' This class is intended to simplify SQL commands.
#'
#' @examples
#' library(RSQL)
#' library(RSQLite)
#' db.name <- getMtcarsdbPath(copy = TRUE)
#' rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
#' select_sql <- rsql$gen_select(
#'   select_fields = "*", # c("wt", "qsec"),
#'   table = "mtcars",
#'   where_values = data.frame(carb = 8)
#' )
#' mtcars.observed <- rsql$execute_select(select_sql)
#' mtcars.observed
#'
#' mtcars.new <- mtcars.observed
#' mtcars.new$carb <- 9
#' insert_sql <- rsql$gen_insert(table = "mtcars", values_df = mtcars.new)
#' rsql$execute_insert(sql_insert = insert_sql)
#'
#' where_values_df <- data.frame(carb = 9)
#' select_sql <- rsql$gen_select(
#'   select_fields = "*", # c("wt", "qsec"),
#'   table = "mtcars",
#'   where_values = data.frame(carb = 8)
#' )
#' mtcars.observed <- rsql$execute_select(select_sql)
#' mtcars.observed
#' @importFrom R6 R6Class
#' @author ken4rab
#' @export
RSQL.class <- R6::R6Class("RSQL", public = list(
  #' @field driver driver  name
  driver = NA,
  #' @field db.name database name
  db.name = NA,
  #' @field user db user
  user = NA,
  #' @field password db password
  password = NA,
  #' @field host db host
  host = NA,
  #' @field port db port
  port = NA,
  # regexp
  #'
  #' @field available.functions for generating select expressions
  available.functions = NA,
  #' @field entity.field.regexp for scrape a field or table expression
  entity.field.regexp = NA,
  #' @field entity.select.regexp for scrape a select expressions expression
  entity.select.regexp = NA,
  # state
  #' @field conn  The connection handler
  conn = NULL,
  #' @field valid.conn Checks if connection is valid
  valid.conn = NULL,
  #' @field last.query The last query
  last.query = NA,
  #' @field last.rs  The last resultset
  last.rs = NULL,
  #' @field results.class  Expected class for db results for running dbClearResult
  results.class = NULL,
  # counters
  #' @field select.counter  An instance select counter
  select.counter = 0,
  #' @field insert.counter  An instance insert counter
  insert.counter = 0,
  #' @field update.counter  An instance update counter
  update.counter = 0,
  #' @field delete.counter   An instance delete counter
  delete.counter = 0,
  #' @field command.counter   An instance command counter
  command.counter = 0,
  #' @field clear.rs.counter   An instance clear.rs.counter
  clear.rs.counter = 0,
  #' @field tz local timezone
  tz = NA,
  #' @field logger is conigured logger for current class
  logger = NA,
  #' @description
  #' Initializes a connection
  #' @param drv driver name
  #' @param dbname database name
  #' @param user user name
  #' @param password password
  #' @param host host name
  #' @param port port number
  #' @param tz   actual time zone
  initialize = function(drv, dbname,
                        user = NULL, password = NULL, host = NULL, port = NULL,
                        tz = Sys.timezone()) {
    self$db.name <- dbname
    self$driver <- drv
    self$user <- user
    self$password <- password
    self$host <- host
    self$port <- port
    self$tz <- tz
    self$logger <- genLogger(self)
    self$checkConnection()
    self$valid.conn
  },
  #' @description
  #' Function which check if db connection is valid
  #' @return conn object
  checkConnection = function()
  {
    logger <- getLogger(self)
    self$valid.conn <- dbCanConnect(
      drv = self$driver, dbname = self$db.name,
      user = self$user, password = self$password,
      host = self$host, port = self$port
    )
    if (!self$valid.conn){
      str(self$valid.conn)
      logger$warn("db connection is not valid",
                  message = attr(self$valid.conn, "reason"))
    }
    self$valid.conn
  },
  #' @description
  #' Function which connects to database
  #' @return conn object
  connect = function() {
    if (is.null(self$conn)) {
      if (!self$valid.conn) {
        stop("Database connection is not valid. Check status and credentials!")
      }
      timezone.statement <- ""
      # Postgres
      if (inherits(self$driver, "PqDriver")){
        #timezone.statement <- paste("-c timezone=", Sys.timezone(), "", sep = "")
      }
      self$conn <- dbConnect(
        drv = self$driver, dbname = self$db.name,
        user = self$user, password = self$password, host = self$host, port = self$port,
        options = timezone.statement,
        timezone = Sys.timezone()
      )
      self$setupResultClassFromDriver()
    }
    else{
      self$clearLastResult()
    }
    self$conn
  },
  #' @description
  #' Infer ResultsClass from corresponding driver. Implemented for SQLiteDriver & PqConnection
  #' @return RSQL object

  setupResultClassFromDriver = function(){
    logger <- getLogger(self)
    # SQLite
    if (inherits(self$driver, "SQLiteDriver")){
      self$results.class <- "SQLiteResult"
    }
    # Postgres
    if (inherits(self$driver, "PqDriver")){
      self$results.class <- "PqResult"
    }
    # Postgres
    if (inherits(self$driver, "MySQLDriver")){
      self$results.class <- "MySQLResult"
    }
    if (is.null(self$results.class)){
      logger$warn("Driver Result class not implemented yet", driver = class(self$driver))
    }
    self
  },
  #' @description
  #' initialize regexp for scraping entities
  #' @param force force setup?
  #' @return regexp for scraping select expressions
  setupRegexp = function(force = FALSE) {
    if (is.na(self$entity.field.regexp) | force) {
      self$available.functions <- c("max", "min", "mean", "tmean", "count")
      self$entity.field.regexp <- "([[:alnum:]\\_]+)"
      available.functions.regexp <- paste(self$available.functions, collapse = "|")
      field.wildcard <- paste("(?:", self$entity.field.regexp, "|\\*)", sep = "")
      self$entity.select.regexp <- paste("^(?:", self$entity.field.regexp, "|(",
        available.functions.regexp, ")\\(",
        self$entity.field.regexp,
        "\\)", "|", "\\*)$",
        sep = ""
      )
      entity.regexp <- "^(?:[[:alnum:]]\\_|(?:max|min|mean|tmean|count)\\([[:alnum:]\\_]+\\)|\\*)$"

      self$entity.field.regexp <- paste("^", self$entity.field.regexp, "$", sep = "")
    }
    self$entity.select.regexp
  },
  #' @description
  #' Class destructor
  finalize = function() {
    message("Finalizing object and disconnecting")
    self$disconnect()
  },
  #' @description
  #' Checks if an entity exists
  #' @param entities entities to check
  #' @param entity.type entity type to check against
  checkEntitiesNames = function(entities, entity.type) {
    self$setupRegexp()
    errors <- NULL
    for (entity in entities) {
      error <- FALSE

      entity.regexp <- NULL
      if (entity.type %in% c("table", "field")) {
        entity.regexp <- self$entity.field.regexp
      }
      if (entity.type == "select") {
        entity.regexp <- self$entity.select.regexp
      }

      grepl(entity.regexp, entity, perl = TRUE)
      gsub(entity.regexp, "====", entity, perl = TRUE)

      if (!grepl(entity.regexp, entity, perl = TRUE)) {
        error <- TRUE
      }
      if (error) {
        errors <- c(errors, entity)
      }
    }
    errors
    if (!is.null(errors)) {
      stop(paste(
        entity.type, " names are not legal:",
        paste(errors, collapse = ",")
      ))
    }
  },
  #' @description
  #' Generates a select
  #' @param select_fields fields to be selected
  #' @param table table to select from
  #' @param where_fields fields in the where clause
  #' @param where_values values to the fields on the where clause
  #' @param group_by fields to group by
  #' @param order_by fields to order by
  #' @param top where does the resultset starts?
  #' @param distinct provides a way to select distinct rows
  gen_select = function(select_fields,
                        table,
                        where_fields = names(where_values),
                        where_values = NULL,
                        group_by = c(),
                        order_by = c(),
                        top = 0,
                        distinct = FALSE) {
    self$connect()
    self$checkEntitiesNames(select_fields, entity.type = "select")
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(c(where_fields, group_by, order_by), entity.type = "field")

    sql_gen_select(
      select_fields = select_fields, table = table,
      where_fields = where_fields,
      where_values = where_values,
      group_by = group_by,
      order_by = order_by,
      top = top,
      distinct = distinct
    )
  },
  #' @description
  #'
  #' Generate  insert statement
  #'
  #' @param values_df The values to insert. Must be defined as data.frame of values
  #' @param table The table to insert into
  #' @param insert_fields the fields to insert into
  gen_insert = function(table, values_df, insert_fields = names(values_df)) {
    self$connect()
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(insert_fields, entity.type = "field")
    sql_gen_insert(table = table, values_df = values_df, insert_fields = insert_fields)
  },
  #' @description
  #'
  #' Generate  insert statement
  #' @param table the table to insert into
  #' @param update_fields the fields to update
  #' @param where_fields a where clause to the insert
  #' @param where_values the values to add to the where clause
  #' @param values the values to update
  gen_update = function(table,
                        update_fields = names(values), values,
                        where_fields = names(where_values), where_values = NULL) {
    self$connect()
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(c(update_fields, where_fields), entity.type = "field")
    sql_gen_update(
      table = table,
      update_fields = update_fields, values = values,
      where_fields = where_fields, where_values = where_values
    )
  },
  #' @description
  #'
  #' Generate a delete statement
  #' @param table the table to insert into
  #' @param where_values the fields to add to the where clause
  #' @param where_fields a where clause to the insert
  gen_delete = function(table, where_fields = names(where_values), where_values = NULL) {
    self$connect()
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(c(where_fields), entity.type = "field")

    sql_gen_delete(table, where_fields, where_values)
  },
  #' @description
  #'
  #' Performs an execution on the database
  #' @param sql_select the sql select statement to perform
  execute_select = function(sql_select) {
    self$connect()
    self$last.query <- sql_select
    self$last.rs <- sql_execute_select(sql_select, dbconn = self$conn)
    self$select.counter <- self$select.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs an update on the database
  #' @param sql_update the sql update statement to perform
  execute_update = function(sql_update) {
    self$connect()
    self$last.query <- sql_update
    self$last.rs <- sql_execute_update(sql_update = sql_update, dbconn = self$conn)
    self$update.counter <- self$update.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs an insert on the database
  #' @param sql_insert the sql insert statement to perform
  execute_insert = function(sql_insert) {
    self$connect()
    self$last.query <- sql_insert
    self$last.rs <- sql_execute_insert(sql_insert = sql_insert, dbconn = self$conn)
    self$insert.counter <- self$insert.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs a select statement, if not exists, executes insert statement
  #' @param sql_select the sql select statement to perform
  #' @param sql_insert the sql insert statement to perform
  execute_get_insert = function(sql_select, sql_insert) {
    self$connect()
    self$last.query <- sql_select
    self$last.rs <- sql_execute_get_insert(
      sql_select = sql_select,
      sql_insert = sql_insert,
      dbconn = self$conn
    )
    self$select.counter <- self$select.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs a command on the database
  #' @param sql_command the sql statement to perform
  execute_command = function(sql_command) {
    self$connect()
    self$last.query <- sql_command
    self$last.rs <- sql_execute_insert(sql_command, dbconn = self$conn)
    self$command.counter <- self$command.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs an deletion on the database
  #' @param sql_delete the sql delete statement to perform
  execute_delete = function(sql_delete) {
    self$connect()
    self$last.query <- sql_delete
    self$last.rs <- sql_execute_delete(sql_delete, dbconn = self$conn)
    self$delete.counter <- self$delete.counter + 1
    self$last.rs
  },
  #' @description
  #'
  #' Performs an insert on the database. This is a composite function
  #' @param table The table
  #' @param fields_uk The fields unique key
  #' @param values_uk The values unique key
  #' @param fields The fields (Not used. Included for compatibility)
  #' @param values The values (Not used. Included for compatibility)
  #' @param field_id The field of the serial id
  retrieve = function(table, fields_uk = names(values_uk), values_uk,
                      fields = names(values), values = NULL,
                      field_id = "id") {
    self$connect()
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(c(fields_uk, field_id), entity.type = "field")
    sql_retrieve(
      table = table, fields_uk = fields_uk, values_uk = values_uk,
      fields = fields, values = values, field_id = field_id,
      dbconn = self$conn
    )
  },
  #' @description
  #'
  #' Obtain id if object exists on the database. Insert object if not.
  #' @param table The table
  #' @param fields_uk The fields unique key
  #' @param values_uk The values unique key
  #' @param fields The fields
  #' @param values The values
  #' @param field_id The field of the serial id
  retrieve_insert = function(table, fields_uk = names(values_uk), values_uk,
                             fields = names(values), values = data.frame(),
                             field_id = "id") {
    self$connect()
    self$checkEntitiesNames(table, entity.type = "table")
    self$checkEntitiesNames(c(fields_uk, fields, field_id), entity.type = "field")
    sql_retrieve_insert(
      table = table, fields_uk = fields_uk, values_uk = values_uk,
      fields = fields, values = values, field_id = field_id,
      dbconn = self$conn
    )
  },
  #' @description
  #'
  #' clearLast Result for avoiding nasty warning
  clearLastResult = function(){
    if (!is.null(self$results.class) & !is.null(self$last.rs)){
      if (inherits(self$last.rs, self$results.class)){
        dbClearResult(self$last.rs)
        self$clear.rs.counter <- self$clear.rs.counter + 1
      }
      self$last.rs <- NULL
    }
  },
  #' getSummary
  #' @description
  #'
  #' get RSQL summary string
  getSummary = function(){
    ret <- paste("RSQL ", class(self$conn),
                 ". selects: ", self$select.counter,
                 ". inserts: ", self$insert.counter,
                 ". updates: ", self$update.counter,
                 ". delete: ", self$delete.counter,
                 ". commands: ", self$command.counter,
                 ". clearRS: ", self$clear.rs.counter, sep = "")
    ret
  },
  #' @description
  #'
  #' Disconnects the instance from the database
  disconnect = function() {
    if (!is.null(self$conn)) {
      self$clearLastResult()
      DBI::dbDisconnect(self$conn)
      self$conn <- NULL
      self$results.class <- NULL
    }
  }
))

#' Produces a RSQL object
#'
#' @param drv Driver name
#' @param dbname Database name
#' @param user Database user name
#' @param password Database password
#' @param host Database host
#' @param port Database port
#' @author ken4rab
#' @export
createRSQL <- function(drv, dbname, user = NULL, password = NULL, host = NULL, port = NULL) {
  RSQL.class$new(drv, dbname, user, password, host, port)
}


#' Executes a statement on the database.
#'
#' @import lgr
#' @param sql_insert The SQL String
#' @param dbconn The Database Connection to run the query against
#' @author ken4rab
sql_execute_insert <- function(sql_insert, dbconn) {
  sql_insert <- gsub(",NA", ",NULL", sql_insert)
  sql_insert <- gsub(", NA", ",NULL", sql_insert)
  sql_insert <- paste(sql_insert, ";", sep = "")
  # ret <- DBI::dbSendQuery(dbconn, sql_insert)
  ret <- DBI::dbSendStatement(dbconn, sql_insert)

  lgr$trace(sql_insert)

  # data <- fetch(rs,n=-1) print(res)
  if (length(ret) > 0) {
    # TODO check if OK

    # if (nchar(ret[1])>5){ print(paste('for processing', id_process, ' in Insert
    # No', contadorInserts, ' an error is inserted in database'))
    # print(paste(sql_insert)) print(paste('Error:',res[1]))
    # insertarErrorEnBD(id_process, contadorInserts, id_ciudad, anio, mes,
    # sql_insert, res[1],context) }
  }
  ret
}

#' Executes an update on the database
#'
#' @param sql_update The update SQL
#' @param dbconn The Database Connection to run the query against
#' @author ken4rab
sql_execute_update <- function(sql_update, dbconn = NULL) {
  ret <- DBI::dbSendQuery(dbconn, sql_update)
  ret
}


#' sql_execute_delete
#'
#' Executes a delete on the Database
#'
#' @param sql_delete The delete SQL
#' @param dbconn The Database Connection to run the query against
#' @author ken4rab
sql_execute_delete <- function(sql_delete, dbconn = NULL) {
  sql_delete <- gsub(",NA", ",NULL", sql_delete)
  sql_delete <- gsub(", NA", ",NULL", sql_delete)
  ret <- DBI::dbExecute(dbconn, sql_delete)
  lgr$trace(sql_delete)
  ret
}

#' sql_execute_select
#' @description
#' Executes a select on the database
#'
#' @import DBI
#' @param sql_select The delete SQL
#' @param dbconn The Database Connection to run the query against
#' @author ken4rab
sql_execute_select <- function(sql_select, dbconn = NULL) {
  # debug sql_select <- 'select price from v_quotes_id_completed where
  # symbol='alua' order by date DESC LIMIT 1' sql_select <- 'select * from
  # v_quotes'
  sql_select <- gsub(",NA", ",NULL", sql_select)
  sql_select <- gsub(", NA", ",NULL", sql_select)
  ret <- DBI::dbGetQuery(dbconn, sql_select)
  #BI:::dbClearResult(ret)
  ret
}

#' sql_execute_get_insert
#' @description
#' Executes the insert statement
#'
#' @param dbconn The db connection
#' @param sql_select The SQL select query
#' @param sql_insert The SQL insert query
#' @param ... other variables to considered.
#' @author ken4rab
sql_execute_get_insert <- function(dbconn, sql_select, sql_insert, ...) {
  ret <- sql_execute_select(sql_select, dbconn = dbconn)
  if (nrow(ret) == 0) {
    insert.rs <- sql_execute_insert(sql_insert, dbconn = dbconn)
    dbClearResult(insert.rs)
    ret <- sql_execute_select(sql_select, dbconn = dbconn)
  }
  ret[1, ]
}


#' Determines string type which needs quotes in an SQL statement
#'
#' @param text The text to test
#' @author ken4rab
needs_quotes <- function(text) {
  class(text) %in% c("Date", "character")
}

#' Determines if the string is quoted or not
#'
#' @param text The text to test
#' @param quotes_symbols The quotation characters
#' @author ken4rab
is_quoted <- function(text, quotes_symbols = "'") {
  ret <- TRUE
  i <- 1
  if (!is.na(text)) {
    if (!is.null(text)) {
      ret <- FALSE
      while (!ret & i <= length(quotes_symbols)) {
        quotes <- quotes_symbols[i]
        ret <- substr(text, 1, 1) == quotes &
          substr(text, nchar(text), nchar(text)) == quotes
        i <- i + 1
      }
    }
  }
  ret
}

#' Stuff quote symbol from text
#'
#' @param unquoted.text The unquoted string to stuff quotes from.
#' @param quote The quoting symbol. Default is '
#' @author ken4rab
stuff_quote <- function(unquoted.text, quote = "'") {
  if (is_quoted(unquoted.text)) {
    stop(paste("stuff_quote function cannot be called with quoted text", unquoted.text))
  }
  stuffed.text <- gsub(quote, paste(quote, quote, sep = ""), unquoted.text)
  stuffed.text
}

#' Removes the quotes from the string
#'
#' @param text The string to remove the quotes from.
#' @author ken4rab
dequote <- function(text) {
  substr(text, 2, nchar(text) - 1)
}

#' This functions remove original quotes and sets validated quotes for corresponding db.
#' If it had no quotes, will only put corresponding quotes symbols
#'
#' @param text The string
#' @param quotes The quotes
#' @author ken4rab
re_quote_alt <- function(text, quotes = "'") {
  text <- unlist(text)
  ret <- vapply(text, FUN = function(x) {
    if (!is.na(x)) {
      if (!is.numeric(x)) {
        quote <- FALSE
        if (is_quoted(x, quotes_symbols = "\"")) {
          x <- dequote(x)
          quote <- TRUE
        }
        if (is_quoted(x, quotes_symbols = "'")) {
          x <- dequote(x)
          quote <- TRUE
        }
        if (quote) {
          x <- paste(quotes, x, quotes, sep = "")
        }
      }
    }
    as.character(x)
  }, FUN.VALUE = character(length(1)))
  unname(ret)
}

#' This functions remove original quotes and sets validated quotes for corresponding db.
#' If it had no quotes, will only put corresponding quotes symbols
#'
#' @param text The string
#' @param quotes The quotes
#' @author ken4rab
re_quote <- function(text, quotes = "'") {
  text <- as.character(text)
  if (!is.na(text)) {
    quote <- FALSE
    if (!is_quoted(text, quotes_symbols = "\"")) {
      quote <- TRUE
    }
    if (is_quoted(text, quotes_symbols = "'")) {
      text <- dequote(text)
      quote <- TRUE
    }
    if (quote) {
      text <- paste(quotes, text, quotes, sep = "")
    }
  }
  text
}
#' add_quotes
#' @description
#' Adds quotes to a string
#'
#' @param text The string to quote
#' @author ken4rab
#' @export
add_quotes <- function(text) {
  ret <- sapply(text, FUN = re_quote)
  names(ret) <- NULL
  ret
}

#' rm_quotes
#' @description
#' Removes quotes from the String
#'
#' @param text The string to remove quotes from
#' @param quotes Quote characters
#' @author ken4rab
rm_quotes <- function(text, quotes = "'") {
  if (!is.na(text)) {
    unquoted.text <- text
    if (quotes == substr(text, 1, 1) & quotes == substr(text, nchar(text), nchar(text))) {
      text <- substr(text, 2, nchar(text) - 1)
      unquoted.text <- text
    }
    text <- stuff_quote(unquoted.text)
  }
  text
}


#' stuff_df_quoted
#' @description
#' stuff quote characters in quoted or not quoted df for DSL or DML operations
#'
#' @param text.df Data Frame with corresponding values and fields as colnames
#' @author ken4rab
stuff_df_quoted <- function(text.df) {
  if (!is.null(text.df)) {
    if (!is.data.frame(text.df)) {
      stop(paste("text.df must be a data.frame but is", class(text.df)[1]))
    } else {
      process <- nrow(text.df) > 0
    }
    if (process) {
      cols.quoted <- apply(text.df, MARGIN = 2, FUN = function(x) vapply(x, FUN = is_quoted, FUN.VALUE = logical(1)))
      cols.need.quotes <- apply(text.df, MARGIN = 2, FUN = function(x) vapply(x, FUN = needs_quotes, FUN.VALUE = logical(1)))
      # TODO warning when an unneeded quotes are using quotes?
      cols.need.quotes <- cols.quoted | cols.need.quotes
      if (nrow(text.df) > 1) {
        cols.need.quotes.min <- apply(cols.need.quotes, MARGIN = 2, FUN = min)
        cols.need.quotes.max <- apply(cols.need.quotes, MARGIN = 2, FUN = max)
      } else {
        cols.need.quotes.min <- cols.need.quotes
        cols.need.quotes.max <- cols.need.quotes
      }
      if (min(cols.need.quotes.min == cols.need.quotes.max) == 0) {
        unsound.columns <- which(cols.need.quotes.min != cols.need.quotes.max)
        stop("unsound quoting strategy in columns", paste(names(text.df)[unsound.columns], collapse = ","))
      }

      for (i in seq_len(nrow(text.df))) {
        text.df[i, ] <- sapply(text.df[i, ], FUN = rm_quotes)
        # Cannot apply vapply if NA value is in scope
        # text.df[i, ] <- vapply(text.df[i, ], FUN = rm_quotes, FUN.VALUE = character(1))
      }
      for (j in seq_len(ncol(text.df))) {
        if (cols.need.quotes.min[j]) {
          text.df[, j] <- sapply(text.df[, j], FUN = re_quote)
          # Cannot apply vapply if NA value is in scope
          # text.df[, j] <- vapply(text.df[, j], FUN = re_quote, FUN.VALUE = character(1))
        }
      }
    }
  }
  text.df
}


#' is.POSIXct
#' @description
#' This function returns true if x is a POSIXct object type
#'
#' @param x value to be checked as a POSIXct
#' @author ken4rab
is.POSIXct <- function(x) inherits(x, "POSIXct")

#' times_to_utc
#' @description
#' This function converts POSIXct columns to UTC for inserting in a database
#'
#' @param values.df Data Frame with corresponding values and fields as colnames
#' @author ken4rab
times_to_utc <- function(values.df)
{
  if (nrow(values.df) > 0){
    for (cc in names(values.df))
    {
      if(is.POSIXct(values.df[1,cc]))
      {
        values.df[, cc] <- as.POSIXct(values.df[, cc], tz = "UTC")
      }
    }
  }
  values.df
}

#' assessRSqlDf
#' @description
#' This function prepares data frame to be inserted in a db
#'
#' @param values.df Data Frame with corresponding values and fields as colnames
#' @author ken4rab
assessRSqlDf <- function(values.df)
{
  values.df <- times_to_utc(values.df)
  values.df <- stuff_df_quoted(text.df = values.df)
  values.df
}

#' rm_vector_quotes
#' @description
#' Removes quotes from data.frame columns
#'
#' @param text.vector The text vector to remove quotes from.
#' @author ken4rab
rm_vector_quotes <- function(text.vector) {
  ret <- sapply(text.vector, FUN = rm_quotes)
  names(ret) <- NULL
  ret
}


#' add_grep_exact_match
#'
#' @param text TEST
#' @author ken4rab
add_grep_exact_match <- function(text) {
  text <- gsub("(\\^|\\%)", "\\\\\\1", text)
  paste("^", text, "$", sep = "")
}

#' sql_gen_delete
#' @description
#' Generates a Delete Statement
#'
#' @param table The table from which the delete statement will be generated
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @author ken4rab
sql_gen_delete <- function(table, where_fields = names(where_values), where_values = NULL) {
  where_values.df <- as.data.frame(where_values)
  names(where_values.df) <- where_fields
  #where_values.df <- stuff_df_quoted(where_values.df)
  where_values.df <- assessRSqlDf(where_values.df)
  sql_where <- sql_gen_where(where_fields, where_values.df)
  ret <- paste("delete from", table, sql_where)
  ret
}

#' sql_gen_select
#' @description
#' Generates a Select Statement
#'
#' @param select_fields The fields to be selected
#' @param table The table to be used in the select
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @param group_by Group by fields
#' @param order_by Order by fields
#' @param top Retrieve top records
#' @param distinct it adds a distinct clause to the query.
#' @author ken4rab
sql_gen_select <- function(select_fields, table,
                           where_fields = names(where_values),
                           where_values = NULL,
                           group_by = c(),
                           order_by = c(),
                           top = 0,
                           distinct = FALSE) {
  where_values.df <- as.data.frame(where_values)
  names(where_values.df) <- where_fields


  #where_values.df <- stuff_df_quoted(where_values.df)
  where_values.df <- assessRSqlDf(where_values.df)

  separator <- ""
  sql_select_fields <- ""
  for (f in select_fields) {
    sql_select_fields <- paste(sql_select_fields, separator, f, sep = "")
    separator <- ", "
  }
  if (distinct) {
    sql_select_fields <- paste("distinct", sql_select_fields)
  }
  sql_where <- sql_gen_where(where_fields, where_values.df)
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
  if (nchar(sql_order_by) > 0) {
    ret <- paste(ret, "order by", sql_order_by)
  }
  if (top > 0) {
    ret <- paste(ret, "limit", top)
  }
  ret <- replaceNAwithNULL(ret)
  ret <- trimws(ret)
}

#' sql_gen_where
#' @description
#' Generates a where statement to be used on a SQL statement.
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @importFrom utils str
#' @author ken4rab
sql_gen_where <- function(where_fields = names(where_values), where_values) {
  check_fields_values(fields = where_fields, values = where_values, min.length = 0)
  ret <- ""
  process <- !is.null(where_fields) & !is.null(where_values)
  if (process) {
    if (is.data.frame(where_values)) {
      process <- nrow(where_values) > 0
    } else {
      if (!is.null(where_fields)) {
        stop(paste(gettext("sql_lib.no_where_values_specified", domain = "R-rsql")))
      }
      if (!is.null(where_values)) {
        stop(paste(gettext("sql_lib.no_where_values_specified", domain = "R-rsql")))
      }
    }
  }
  if (process) {
    # Asserts with values
    if (!is.vector(where_fields)) {
      stop(paste(gettext("sql_lib_where_files_has_to_be_a_vector",
        domain = "R-rsql"
      ), str(where_fields)))
    }
    if (!is.list(where_values)) {
      if (is.vector(where_values)) {
        where_values <- data.frame(matrix(where_values,
          byrow = TRUE,
          ncol = length(where_values)
        ),
        stringsAsFactors = FALSE
        )
      }
    }
    if (length(where_fields) != ncol(where_values)) {
      stop(paste(
        gettext("sql_lib.where_fields_num_not_eq_where_values_num", domain = "R-sql"), length(where_fields), "!=",
        length(where_values), paste(where_fields, collapse = ","), paste(where_values,
          collapse = ","
        )
      ))
    }
    # if strings values, add '
    for (col in names(where_values)) {
      where_values_col <- where_values[, col]
      if (inherits(where_values_col, "factor")) {
        where_values_col <- as.character(where_values_col)
      }
      # Removed
      # if (max(is.character(where_values_col)) == 1) {
      #     # TODO extend to multiple columns
      #     # if there is at least one value character in column remove ' for normalization
      #     # and adding after
      #     new.values <- paste("'", sub("\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
      #       "\\1", where_values[, col]), "'", sep = "")
      #     lgr$trace(paste("col", col, "is character. Replacing values",
      #       paste(where_values[, col], collapse = ","), "with values", paste(new.values,
      #         collapse = ",")))
      #     where_values[, col] <- new.values
      # }
    }

    ret <- ""
    if (length(where_fields) > 0 & !(length(where_fields) == 1 & nchar(where_fields[1]) ==
      0)) {
      where_values <- as.data.frame(where_values,
        nrow = nrow(where_values) / length(where_fields),
        stringsAsFactors = FALSE
      )
      # if (nrow(where_values) > 2){
      ret <- sql_gen_where_list(where_fields = where_fields, where_values = where_values)
      # }
      # else{
      #  ret <- sql_gen_where_or(where_fields, where_values)
      # }
    }
  }
  ret
}

#' sql_gen_where_list
#' @description
#' Generates a where list statement to be used on a SQL statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @author ken4rab
sql_gen_where_list <- function(where_fields, where_values) {
  sql_where <- ""
  comma.sep <- ", "
  if (length(where_fields) > 0) {
    separator <- ""
    sql_where <- "where ("
    for (f in where_fields) {
      sql_where <- paste(sql_where, separator, f, sep = "")
      separator <- comma.sep
    }
    sql_where <- paste(sql_where, ") in ", sep = "")
    list_where <- ""
    separator_list <- ""
    for (v in seq_len(nrow(where_values))) {
      i <- 1
      separator <- ""
      sql_row_where <- ""
      for (f in where_fields) {
        if (is.na(where_values[v, i])) {
          value <- paste(":label_", f, ":", sep = "")
        } else {
          value <- where_values[v, i]
        }

        if (is.character(value)) {
          value <- add_quotes(value)
        }
        sql_row_where <- paste(sql_row_where, separator, value, sep = "")
        separator <- comma.sep
        i <- i + 1
      }
      if (i > 2) {
        sql_row_where <- paste("(", sql_row_where, ")", sep = "")
      }
      list_where <- paste(list_where, separator_list, sql_row_where, sep = "")
      separator_list <- comma.sep
    }
    sql_where <- paste(sql_where, "(", list_where, ")", sep = "")
  }
  sql_where
}

#' sql_gen_where_or
#' @description
#' Generates a where (or) statement to be used on a SQL statement.
#'
#' @param where_fields The fields used in the where section
#' @param where_values The values used in the where section
#' @author ken4rab
sql_gen_where_or <- function(where_fields = names(where_values), where_values) {
  sql_where <- ""
  if (length(where_fields) > 0) {
    sql_where <- "where"
    separator_where <- ""
    for (v in seq_len(nrow(where_values))) {
      i <- 1
      separator <- ""
      sql_row_where <- ""
      for (f in where_fields) {
        if (is.na(where_values[v, i])) {
          value <- paste(":label_", f, ":", sep = "")
        } else {
          value <- where_values[v, i]
        }
        if (is.character(value)) {
          value <- add_quotes(value)
        }
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



#' sql_gen_insert
#' @description
#' Generates an insert statement.
#'
#' @param table The table to be affected
#' @param insert_fields The fields to insert
#' @param values_df The values to insert. Must be defined as data.frame of values
#' @author ken4rab
sql_gen_insert <- function(table, values_df, insert_fields = names(values_df)) {
  values.df <- as.data.frame(values_df)
  names(values.df) <- insert_fields
  values.df <- assessRSqlDf(values.df)
  #values.df <- times_to_utc(values.df)
  #values.df <- stuff_df_quoted(values.df)
  if (length(values_df) > 1 & !inherits(values_df, "data.frame")) {
    stop("Values must be defined as data.frames with same size of columns")
  }
  # Converts all factors to strings
  values.df <- as.data.frame(lapply(values.df, as.character))

  if (length(insert_fields) != ncol(values_df)) {
    stop(paste(
      gettext("sql_lib.incompatible_fields_and_data", domain = "R-rsql"), length(insert_fields), gettext("sql_lib.not_eq", domain = "R-rsql"),
      ncol(values_df), paste(insert_fields, collapse = ";"), paste(values.df, collapse = ";")
    ))
  }
  separator <- ""
  sql_insert_fields <- ""
  for (f in insert_fields) {
    sql_insert_fields <- paste(sql_insert_fields, separator, f, sep = "")
    separator <- ", "
  }
  sql_values <- ""
  separator_rows <- ""
  for (i in seq_len(nrow(values.df))) {
    sql_values_row <- ""
    separator <- ""
    for (j in seq_len(length(insert_fields))) {
      if (is.na(values.df[i, j])) {
        value <- "NA"
      } else {
        value <- values.df[i, j]
        if (is.character(value)) {
          value <- add_quotes(value)
        }
      }

      sql_values_row <- paste(sql_values_row, separator, value, sep = "")
      separator <- ", "
    }
    lgr$trace(sql_values_row)
    sql_values <- paste(sql_values, separator_rows, "(", sql_values_row, ")")
    separator_rows <- ", "
  }
  ret <- paste("insert into ", table, "(", sql_insert_fields, ") values ", sql_values,
    sep = ""
  )
  ret <- replaceNAwithNULL(ret)
  ret
}

#' replaceNAwithNULL
#' @description
#' Replace NA with NULL in sql statement
#'
#' @param sql.code code to replace NA with NULL
#' @author ken4rab
replaceNAwithNULL <- function(sql.code) {
  sql.code <- gsub("(,( )?)?NA", "\\1NULL", sql.code)
  # sql.code <- gsub(", NA", ", NULL", sql.code)
  sql.code
}


#' sql_gen_update
#' @description
#' Generates an update statement
#'
#' @param table The table to update
#' @param update_fields The fields to update
#' @param values The values to update
#' @param where_fields The fields for where statement
#' @param where_values The values for where statement
#' @author ken4rab
sql_gen_update <- function(table, update_fields = names(values), values, where_fields = names(where_values), where_values) {
  check_fields_values(fields = update_fields, values = values, min.length = 1)
  check_fields_values(fields = where_fields, values = where_values, min.length = 0)

  values.df <- as.data.frame(values)
  names(values.df) <- update_fields
  values.df <- assessRSqlDf(values.df)
  #values.df <- stuff_df_quoted(text.df = values.df)
  #values.df <- times_to_utc(values.df)

  where_values.df <- as.data.frame(where_values)
  names(where_values.df) <- where_fields
  #where_values.df <- stuff_df_quoted(text.df = where_values.df)
  where_values.df <- assessRSqlDf(where_values.df)
  sql_where <- sql_gen_where(where_fields, where_values.df)
  update.fields.sql <- paste(update_fields, collapse = ",")
  update.values.sql <- paste(add_quotes(values.df), collapse = ",")

  if (length(values.df) > 1) {
    # Was checked fields and values has same length
    update.fields.sql <- paste("(", update.fields.sql, ")", sep = "")
    update.values.sql <- paste("(", update.values.sql, ")", sep = "")
  }
  ret <- paste("update ", table, " set ", update.fields.sql,
    "=", update.values.sql,
    " ", sql_where,
    sep = ""
  )
  ret <- gsub(",\'{,1}NA\'{,1}", ",NULL", ret)
  ret <- replaceNAwithNULL(ret)
  ret
}


#' trim_leading
#' @description
#' Returns string w/o leading whitespace
#'
#' @param x The string
#' @author ken4rab
trim_leading <- function(x) sub("^\\s+", "", x)

#' trim_trailing
#' @description
#' Returns string w/o trailing whitespace
#'
#' @param x The string
#' @author ken4rab
trim_trailing <- function(x) sub("\\s+$", "", x)


#' Returns string w/o leading or trailing whitespace
#'
#' @param x The string
#' @author ken4rab
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#' rename_col
#' @description
#' renames a column on a data.frame
#'
#' @param df The date.frame
#' @param name The name of the column
#' @param replace_name The new name of the column
#' @author ken4rab
rename_col <- function(df, name, replace_name) {
  i <- which(names(df) == name)
  names(df)[i] <- replace_name
  df
}

#' cbind_coerced
#'
#' @param ... The parameters
#' @author ken4rab
cbind_coerced <- function(...) {
  ret <- cbind(...)
  if ("stringsAsFactors" %in% names(ret)) {
    ret <- ret[, -which(names(ret) == "stringsAsFactors")]
  }
  ret
}


#' Checks that the columns are in the data.frame
#'
#' @param dataframe The data.frame
#' @param columns The columns to check
#' @author ken4rab
df_verify <- function(dataframe, columns) {
  ret <- NULL
  dataframe_names <- names(dataframe)
  for (column in columns) {
    if (!column %in% dataframe_names) {
      ret <- c(ret, column)
    }
  }
  if (length(ret) > 0) {
    stop(paste(
      gettext("sql_lib.missing_columns_in_dataframe", domain = "R-rsql"), paste(ret, collapse = ","), "df",
      paste(dataframe_names, collapse = ",")
    ))
  }
}


#' Retrieves Statement
#'
#' @param table The table
#' @param fields_uk The fields unique key
#' @param values_uk The values unique key
#' @param field_id The field of the serial id
#' @param fields The fields (Not used. Included for compatibility)
#' @param values The values (Not used. Included for compatibility)
#' @param dbconn The database connection
#' @author ken4rab
sql_retrieve <- function(table, fields_uk = names(values_uk), values_uk,
                         fields = names(values), values = NULL,
                         field_id = "id", dbconn = NULL) {
  ret <- NULL
  values_uk <- as.data.frame(values_uk)
  names(values_uk) <- fields_uk
  #values_uk <- stuff_df_quoted(text.df = values_uk)
  values_uk <- assessRSqlDf(values.df = values_uk)

  if (!is.null(values)) {
    values <- as.data.frame(values)
    names(values) <- fields
    values <- stuff_df_quoted(text.df = values)
  }

  for (i in seq_len(nrow(values_uk))) {
    # value_uk <- as.character(values_uk[i,]) value <- as.character(values[i,])
    value_uk <- as.data.frame(values_uk[i, ])

    select_statement <- sql_gen_select(field_id, table,
      where_fields = fields_uk,
      where_values = value_uk
    )
    lgr$trace(paste("verifying", select_statement, ":"))
    row <- sql_execute_select(select_statement, dbconn = dbconn)
    lgr$trace("Retrieved", rows = nrow(row))
    ret <- c(ret, as.numeric(row[, field_id]))
    i <- i + 1
  }
  ret
}


#' Retrieves or insert Statement
#'
#' @param table The table
#' @param fields_uk The fields unique key
#' @param values_uk The values unique key
#' @param fields The fields
#' @param values The values
#' @param field_id The field of the serial id
#' @param dbconn The database connection
#' @author ken4rab
sql_retrieve_insert <- function(table, fields_uk = names(values_uk), values_uk,
                                fields = names(values), values = NULL,
                                field_id = "id", dbconn = NULL) {
  ret <- NULL
  values_uk <- as.data.frame(values_uk)
  names(values_uk) <- fields_uk
  values <- as.data.frame(values)
  names(values) <- fields
  values_uk <- assessRSqlDf(values.df = values_uk)
  values <- assessRSqlDf(values.df = values)
  #values_uk <- stuff_df_quoted(text.df = values_uk)
  #values <- stuff_df_quoted(text.df = values)

  if (nrow(values) > 0 & nrow(values) != nrow(values_uk)) {
    stop(paste(gettext("sql_lib.error_nrows_values_uk_neq_nrows_values", domain = "R-rsql"), nrow(values_uk), nrow(values)))
  }


  for (i in seq_len(nrow(values_uk))) {
    # value_uk <- as.character(values_uk[i,]) value <- as.character(values[i,])
    value_uk <- as.data.frame(values_uk[i, ])
    value <- values[i, ]
    values_insert <- cbind_coerced(value_uk, value)

    select_statement <- sql_gen_select(field_id, table,
      where_fields = fields_uk,
      where_values = value_uk
    )

    lgr$trace(paste("verifying", select_statement, ":"))
    insert_statement <- sql_gen_insert(table,
      insert_fields = c(fields_uk, fields),
      values_df = values_insert
    )
    row <- sql_execute_select(select_statement, dbconn = dbconn)
    lgr$trace("Retrieved", rows = nrow(row))
    if (nrow(row) == 0) {
      lgr$trace(paste("executing", insert_statement))
      # TODO solve dbClearResult in case of sql_retrieve_insert
      #dbClearResult(row)
      insert.rs <- sql_execute_insert(insert_statement, dbconn = dbconn)
      dbClearResult(insert.rs)
      row <- sql_execute_select(select_statement, dbconn = dbconn)
    }
    ret <- c(ret, as.numeric(row[, field_id]))
    #dbClearResult(row)
    i <- i + 1
  }
  ret
}


#' Generates a Joined Query
#' TODO integrate with external functionality
#' @param dw_definition TEST
#' @param recipe TEST
#' @param indicator_fields TEST
#' @noRd
#' @author ken4rab
sql_gen_joined_query <- function(dw_definition, recipe, indicator_fields) {
  # sql_gen_select <- function(select_fields, table, where_fields='',
  # where_values=NULL,group_by=c()){
  sql_select_fields <- rep("", length(indicator_fields))
  sql_from <- ""
  sql_where <- ""
  ind_i <- 0
  where_sep <- ""
  from_sep <- ""
  for (i in seq_len(nrow(recipe$m_recipe))) {
    current_expression <- recipe$m_recipe[i, ]
    # TODO correct in a dictionary
    alias <- gsub("\\.", "_", current_expression$value)
    for (j in seq_len(length(indicator_fields))) {
      if (current_expression$op == "=") {
        sql_select_fields[j] <- current_expression$value
      } else {
        sql_select_fields[j] <- paste("(", sql_select_fields[j], current_expression$op,
          alias, ".", indicator_fields[j], ")",
          sep = ""
        )
      }
    }
    if (current_expression$value_type == "indicator") {
      ind_i <- ind_i + 1
      if (ind_i == 1) {
        first_alias <- alias
      }
      sql_from <- paste(sql_from, from_sep, dw_definition$m_fact_table, " ",
        alias,
        sep = ""
      )
      from_sep <- ","
      # first_field_def <- dw_definition$m_dimensions[1, ]
      for (k in seq_len(nrow(dw_definition$m_dimensions))) {
        current_field_def <- dw_definition$m_dimensions[k, ]
        explicit_value <- ind_i == 1 & nchar(current_field_def$default) ==
          0
        # TODO define in dw_definition fields mapping
        explicit_value <- explicit_value | current_field_def$field == "symbol"
        filter <- ind_i > 1 | current_field_def$field == "symbol"

        # debug explicit_value< <- explicit_value dw_definition< <- dw_definition k< <- k

        if (explicit_value) {
          if (current_expression$value_id > 0) {
            right_value <- current_expression$value_id
          } else {
            right_value < paste("'", current_expression$value, "'", sep = "")
          }
        } else {
          if (filter) {
            right_value <- paste(first_alias, ".", current_field_def$field,
              sep = ""
            )
          }
        }
        if (filter) {
          sql_where <- paste(sql_where, where_sep, alias, ".", current_field_def$field,
            "=", right_value,
            sep = ""
          )
          where_sep <- " and "
        }
        current_field_def
      }
    }
    ret <- paste(
      "select", paste(sql_select_fields, "as", indicator_fields, collapse = ","),
      "from", sql_from, "where", sql_where
    )
  }
  ret
}

#' Parses a where clause.
#'
#' @param where_clause_list The list of params
#' @import lgr
#' @author ken4rab
parse_where_clause <- function(where_clause_list = c()) {
  where_df <- data.frame(
    lhs = character(), comp = character(), rhs = character(),
    stringsAsFactors = FALSE
  )
  names(where_df) <- c("lhs", "comp", "rhs")
  for (where_clause in where_clause_list) {
    where_struct <- strsplit(where_clause, "!=")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], "!=", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
    where_struct <- strsplit(where_clause, "<=")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], "<=", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
    where_struct <- strsplit(where_clause, ">=")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], ">=", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
    where_struct <- strsplit(where_clause, "=")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], "=", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
    where_struct <- strsplit(where_clause, ">")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], ">", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
    where_struct <- strsplit(where_clause, "<")
    if (length(where_struct[[1]]) == 2) {
      where <- data.frame(where_struct[[1]][1], "<", paste("'", sub(
        "\\'([a-zA-Z0-9[:punct:]!'[:space:]]+)\\'",
        "\\1", where_struct[[1]][2]
      ), "'", sep = ""))
      names(where) <- c("lhs", "comp", "rhs")
      where_df <- rbind(where_df, where)
      next
    }
  }
  where_df
}

#' Operator IN for multiple columns
#'
#' @param x vector x
#' @param y vector y
#' @author ken4rab
"%IN%" <- function(x, y) interaction(x) %in% interaction(y)


#' Get package directory
#'
#' Gets the path of package data.
#' @author ken4rab
#' @export
getPackageDir <- function() {
  home.dir <- find.package("RSQL", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata")
  if (!dir.exists(file.path(home.dir, data.subdir))) {
    data.subdir <- "extdata"
  }
  file.path(home.dir, data.subdir)
}

#' getCarsdbPath
#' @param copy a boolean that states whether it should be copied to the home directory or not.
#' @author ken4rab
#' @export
getMtcarsdbPath <- function(copy = TRUE) {
  db.filename <- "mtcars.db"
  source.path <- file.path(getPackageDir(), db.filename)
  if (copy) {
    tmp.dir <- tempdir()
    file.copy(source.path, tmp.dir, overwrite = TRUE)
    ret <- file.path(tmp.dir, db.filename)
  } else {
    ret <- source.path
  }
  ret
}


#' Check fields and values are sound
#'
#' @param fields Fields names to check
#' @param values values to check
#' @param min.length for vectors
#' @author ken4rab
check_fields_values <- function(fields, values, min.length = 0) {
  stopifnot(length(values) >= min.length)
  stopifnot(length(fields) == length(values))
}
