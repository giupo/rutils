#!/usr/bin/env Rscript
library(testthat)

test_check("rutils", reporter = TapReporter)