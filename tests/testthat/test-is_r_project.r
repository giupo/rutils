test_that("is_r_project yields TRUE in an R project", {
  path <- tempdir()
  file.create(file.path(path, c("DESCRIPTION", "NAMESPACE")))
  dir.create(file.path(path, "R"))

  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  expect_true(is_r_project(path))
})

test_that("is_r_project yields FALSE in any other dir", {
  path <- tempdir()
  withr::defer(unlink(path))
  expect_false(is_r_project(path))
})


test_that("is_r_project yields FALSE if a single condition is broken", {
  path <- tempdir()
  file.create(file.path(path, c("DESCRIPTION")))
  dir.create(file.path(path, "R"))
  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  expect_false(is_r_project(path))

  path <- tempdir()
  file.create(file.path(path, c("NAMESPACE")))
  dir.create(file.path(path, "R"))
  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  expect_false(is_r_project(path))

  path <- tempdir()
  file.create(file.path(path, c("NAMESPACE", "DESCRIPTION")))
  withr::defer(unlink(path, recursive = TRUE, force = TRUE))
  expect_false(is_r_project(path))
})