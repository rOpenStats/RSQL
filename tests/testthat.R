library(testthat)
library(DBI)
library(RSQLite)
library(RSQL)
library(lgr)
library(dplyr)

log.levels <- lgr::get_log_levels()

lgr$set_threshold(log.levels["debug"])



test_check("RSQL")
