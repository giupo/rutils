#!/usr/bin/env Rscript
library(testthat)
library(devtools)
library(methods)
library(R6)
# options(env="test")
options(GCLUSTER=F)
load_all()

auto_test("R", "tests/testthat/")
