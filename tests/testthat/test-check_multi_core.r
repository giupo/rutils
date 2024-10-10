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

testthat::test_that("check_multi_core is silent if I have multicore", {
  skip_if_not(require(mockery), "mockery required")


  get_do_pars_mock <- mockery::mock(2)
  logger_mock <- mockery::mock()

  mockery::stub(check_multi_core, "foreach::getDoParWorkers", get_do_pars_mock)
  mockery::stub(check_multi_core, "rutils::.warn", logger_mock)

  check_multi_core()
  mockery::expect_called(get_do_pars_mock, 1)
  mockery::expect_called(logger_mock, 0)
})

testthat::test_that("check_multi_core complaints with one core", {
  skip_if_not(require(mockery), "mockery required")


  get_do_pars_mock <- mockery::mock(1)
  logger_mock <- mockery::mock()

  mockery::stub(check_multi_core, "foreach::getDoParWorkers", get_do_pars_mock)
  mockery::stub(check_multi_core, ".warn", logger_mock)

  check_multi_core()
  mockery::expect_called(get_do_pars_mock, 1)
  mockery::expect_called(logger_mock, 1)
})

testthat::test_that("check_multi_core complaints with if can't get ncores", {
  skip_if_not(require(mockery), "mockery required")

  get_do_pars_mock <- mock(stop("ERRORE"))
  logger_mock <- mock()

  stub(check_multi_core, "foreach::getDoParWorkers", get_do_pars_mock)
  stub(check_multi_core, ".warn", logger_mock)

  check_multi_core()
  expect_called(get_do_pars_mock, 1)
  expect_called(logger_mock, 1)
})