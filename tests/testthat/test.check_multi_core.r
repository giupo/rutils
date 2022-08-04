test_that("if in multicore, no output is displayed", {
  skip_if_not(requireNamespace("mockery"))

  get_par_workers <- mockery::mock(2, cycle = T)
  logger_mock <- mockery::mock()

  mockery::stub(check_multi_core, "foreach::getDoParWorkers", get_par_workers)
  mockery::stub(check_multi_core, ".warn", logger_mock)

  expect_error(check_multi_core(), NA)
  expect_output(check_multi_core(), NA)

  mockery::expect_called(logger_mock, 0)
})

test_that("if in multicore, a warn is displayed", {
  skip_if_not(requireNamespace("mockery"))

  get_par_workers <- mockery::mock(1)
  logger_mock <- mockery::mock()

  mockery::stub(check_multi_core, ".warn", logger_mock)
  mockery::stub(check_multi_core, "foreach::getDoParWorkers", get_par_workers)

  expect_output(check_multi_core(), NA)
  mockery::expect_called(logger_mock, 1)
})

test_that("if can't determine multicore status due error, assume 1 core and go on", {
  skip_if_not(requireNamespace("mockery"))

  get_par_workers <- mockery::mock(function() stop("some error"))
  logger_mock <- mockery::mock()

  mockery::stub(check_multi_core, ".warn", logger_mock)
  mockery::stub(check_multi_core, "foreach::getDoParWorkers", get_par_workers)

  expect_error(check_multi_core(), NA)
  mockery::expect_called(logger_mock, 1)
})