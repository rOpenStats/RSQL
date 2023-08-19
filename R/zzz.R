#' rsql
#'
#' A package to work with SQL datasources in a simple manner
#'
#' @docType package
#' @name RSQL
#' @aliases RSQL-package
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
#' @author Alejandro Baranek <abaranek@dc.uba.ar>, Leonardo Javier Belen <leobelen@gmail.com>
#' Executes code while loading the package.
#'
#' @param libname Library name
#' @param pkgname Package name
# execute onload
.onLoad <- function(libname, pkgname) {

}
