test_that("gen_unique_file works as expected", {
  expect_error(gen_unique_filename(), NA)
})

test_that("gen_unique_file can create file with extension", {
  skip_if_not_installed("mockery")
  expect_error(x <- gen_unique_filename(ext = "txt"), NA)
  expect_true(endsWith(x, ".txt"))
})

test_that("gen_unique_file changes name if file exists", {
  skip_if_not_installed("mockery")

  file_exists_mock <- mockery::mock(TRUE, FALSE, cycle = FALSE)
  logger_mock <- mockery::mock()

  mockery::stub(gen_unique_filename, "file.exists", file_exists_mock)
  expect_error(x <- gen_unique_filename(), NA)

  mockery::expect_called(file_exists_mock, 2)
})