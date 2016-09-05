#!/usr/bin/env Rscript
library(testthat)
library(devtools)
library(methods)
# options(env="test")
options(GCLUSTER=F)
load_all()

.praise <- c(
  "You rock!",
  "You are a coding rockstar!",
  "Keep up the good work.",
  ":)",
  "Woot!",
  "Way to go!",
  "Nice code."
)

auto_test("R", "tests/testthat/")
