#' rsql
#'
#' A package to work with SQL datasources in a simple manner
#'
#' @docType package
#' @name rsql
#' @import R6 futile.logger DBI testthat
#' @importFrom utils str
#' @author Leonardo Javier Belen <leobelen@gmail.com>, Alejandro Baranek <alejandrobaranek@gmail.com>

.onLoad <- function(libname, pkgname) {
  if (!exists("DMLcounter"))
    DMLcounter <<-  0
  if (!exists("sql_inserts"))
    sql_inserts  <<-  data.frame(sql = "", stringsAsFactors = FALSE)
}
