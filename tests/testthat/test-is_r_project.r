test_that("is_r_project yields TRUE in an R project", {
  # NOTE: testthat changes wd to tests/testthat
  #  that's why we go up two levels
  expect_true(is_r_project("../.."))
})

test_that("is_r_project yields FALSE in any other dir", {
  path <- tempdir()
  withr::defer(unlink(path))
  expect_false(is_r_project(path))
})