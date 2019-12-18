#' rsql
#'
#' A package to work with SQL datasources in a simple manner
#'
#' @docType package
#' @name rsql
#' @import R6 lgr DBI
#' @importFrom utils str
#' @author Alejandro Baranek <abaranek@dc.uba.ar>, Leonardo Javier Belen <leobelen@gmail.com>

#' Executes code while loading the package.
#'
#' @param libname Library name
#' @param pkgname Package name
# execute onload
.onLoad <- function(libname, pkgname) {
  if (!exists("DMLcounter"))
    DMLcounter <<-  0
  if (!exists("sql_inserts"))
    sql_inserts  <<-  data.frame(sql = "", stringsAsFactors = FALSE)
}
