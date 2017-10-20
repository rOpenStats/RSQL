# execute onload
.onLoad <- function(libname, pkgname) {
  if (!exists("DMLcounter"))
    DMLcounter <<-  0
  if (!exists("sql_inserts"))
    sql_inserts  <<-  data.frame(sql = "", stringsAsFactors = FALSE)
}
