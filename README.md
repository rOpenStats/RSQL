# RSQL

 <!-- Database Agnostic Package to Generate and Process 'SQL' Queries in R. -->

 Allows the user to generate and execute select, insert, update and delete 'SQL' queries the underlying database without having to explicitly write 'SQL' code. 


| Release | Usage | Development |
|:--------|:------|:------------|
|| [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-blue.svg)](https://cran.r-project.org/) | [![Travis](https://travis-ci.org/rOpenStats/RSQL.svg?branch=master)](https://travis-ci.org/rOpenStats/RSQL) |
| [![CRAN](http://www.r-pkg.org/badges/version/RSQL)](https://cran.r-project.org/package=RSQL) | | [![codecov](https://codecov.io/gh/rOpenStats/RSQL/branch/master/graph/badge.svg)](https://codecov.io/gh/rOpenStats/RSQL) |
|||[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)|

# How to get started
```R
install.packages("RSQL")
```

# How to get started (Development version)

Install the R package using the following commands on the R console:

```R
devtools::install_github("rOpenStats/RSQL", build_opts = NULL)
library(RSQL)
```

# A simple example 

To get started execute the following commands:

```R
# 0.  Load libraries
library(RSQL)

# 1.  RSQL
db.name <- getMtcarsdbPath()
rsql <- createRSQL(drv = RSQLite::SQLite(), dbname = db.name)
query_sql <- rsql$gen_select(
    select_fields = c("*"),
    table = "mtcars")

query_sql <- rsql$gen_select(
    select_fields = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am"),
    table = "mtcars",
    where_fields = "gear",
    where_values = 4)
mtcars.observed <- rsql$execute_select(query_sql)

```

# Troubleshooting


Please note that the 'RSQL' project is released with a [Contributor Code of Conduct](https://github.com/rOpenStats/RSQL/blob/master/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.


<!--[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)-->
