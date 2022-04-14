
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSQL

[![Downloads](http://cranlogs.r-pkg.org/badges/RSQL?color=brightgreen)](https://www.r-pkg.org:443/pkg/RSQL)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RSQL?color=brightgreen)](https://www.r-pkg.org:443/pkg/RSQL)

<!-- Database Agnostic Package to Generate and Process 'SQL' Queries in R. -->

Allows the user to generate and execute select, insert, update and
delete ‘SQL’ queries the underlying database without having to
explicitly write ‘SQL’ code.

| Release                                                                                      | Usage                                                                                                    | Development                                                                                                                                                                                            |
|:---------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                                                                              | [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-blue.svg)](https://cran.r-project.org/) | [![R-CMD-check](https://github.com/rOpenStats/RSQL/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenStats/RSQL/actions)                                                                       |
| [![CRAN](http://www.r-pkg.org/badges/version/RSQL)](https://cran.r-project.org/package=RSQL) |                                                                                                          | [![codecov](https://codecov.io/gh/rOpenStats/RSQL/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rOpenStats/RSQL)                                                                           |
|                                                                                              |                                                                                                          | [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) |

# How to get started

``` r
install.packages("RSQL")
```

# How to get started (Development version)

Install the R package using the following commands on the R console:

``` r
devtools::install_github("rOpenStats/RSQL", build_opts = NULL)
```

# A simple example

To get started execute the following commands:

``` r
# 0.  Load libraries
library(RSQL)
```

``` r

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
query_sql
#> [1] "select mpg, cyl, disp, hp, drat, wt, qsec, vs, am from mtcars where (gear) in ('4')"

rsql$execute_select(query_sql)
#>     mpg cyl  disp  hp drat    wt  qsec vs am
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1
#> 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> 4  24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> 5  22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> 6  19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> 7  17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> 8  32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> 9  30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> 10 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> 11 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> 12 21.4   4 121.0 109 4.11 2.780 18.60  1  1
```

``` r
update_sql <- rsql$gen_update(
    update_fields = c("vs"),
    values = 1,
    table = "mtcars",
    where_fields = "gear",
    where_values = 4)
update_sql
#> [1] "update mtcars set vs='1' where (gear) in ('4')"
rsql$execute_update(update_sql)
#> <SQLiteResult>
#>   SQL  update mtcars set vs='1' where (gear) in ('4')
#>   ROWS Fetched: 0 [complete]
#>        Changed: 12
rsql$execute_select(query_sql)
#> Warning: Closing open result set, pending rows
#>     mpg cyl  disp  hp drat    wt  qsec vs am
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  1  1
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  1  1
#> 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1
#> 4  24.4   4 146.7  62 3.69 3.190 20.00  1  0
#> 5  22.8   4 140.8  95 3.92 3.150 22.90  1  0
#> 6  19.2   6 167.6 123 3.92 3.440 18.30  1  0
#> 7  17.8   6 167.6 123 3.92 3.440 18.90  1  0
#> 8  32.4   4  78.7  66 4.08 2.200 19.47  1  1
#> 9  30.4   4  75.7  52 4.93 1.615 18.52  1  1
#> 10 33.9   4  71.1  65 4.22 1.835 19.90  1  1
#> 11 27.3   4  79.0  66 4.08 1.935 18.90  1  1
#> 12 21.4   4 121.0 109 4.11 2.780 18.60  1  1
```

``` r
insert.df <- c(4, rep(99, 9))
names(insert.df) <- c("gear", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am")
insert.df <- as.data.frame(t(insert.df))
insert_sql <- rsql$gen_insert(
    values = insert.df,
    table = "mtcars")
rsql$execute_insert(insert_sql)
#> <SQLiteResult>
#>   SQL  insert into mtcars(gear, mpg, cyl, disp, hp, drat, wt, qsec, vs, am) values   ( '4', '99', '99', '99', '99', '99', '99', '99', '99', '99' );
#>   ROWS Fetched: 0 [complete]
#>        Changed: 1
rsql$execute_select(query_sql)
#> Warning: Closing open result set, pending rows
#>     mpg cyl  disp  hp  drat     wt  qsec vs am
#> 1  21.0   6 160.0 110  3.90  2.620 16.46  1  1
#> 2  21.0   6 160.0 110  3.90  2.875 17.02  1  1
#> 3  22.8   4 108.0  93  3.85  2.320 18.61  1  1
#> 4  24.4   4 146.7  62  3.69  3.190 20.00  1  0
#> 5  22.8   4 140.8  95  3.92  3.150 22.90  1  0
#> 6  19.2   6 167.6 123  3.92  3.440 18.30  1  0
#> 7  17.8   6 167.6 123  3.92  3.440 18.90  1  0
#> 8  32.4   4  78.7  66  4.08  2.200 19.47  1  1
#> 9  30.4   4  75.7  52  4.93  1.615 18.52  1  1
#> 10 33.9   4  71.1  65  4.22  1.835 19.90  1  1
#> 11 27.3   4  79.0  66  4.08  1.935 18.90  1  1
#> 12 21.4   4 121.0 109  4.11  2.780 18.60  1  1
#> 13 99.0  99  99.0  99 99.00 99.000 99.00 99 99
```

# Troubleshooting

Please note that the ‘RSQL’ project is released with a [Contributor Code
of
Conduct](https://github.com/rOpenStats/RSQL/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

<!--[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)-->
