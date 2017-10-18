library(testthat)
library(DBI)
library(RSQLite)
library(rsql)

futile.logger::flog.threshold(futile.logger::DEBUG)

test_check("rsql")
