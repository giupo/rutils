
testthat::test_that("prev_quarter does what it claims", {
  skip_if_not(requireNamespace("mockery"), "mockery required")
  sys_date_mock <- mockery::mock(
    as.Date("2020-02-01"),
    as.Date("2020-05-01"),
    as.Date("2020-08-01"),
    as.Date("2020-11-01"),
  )

  mockery::stub(prev_quarter, "Sys.Date", sys_date_mock)

  expect_equal(prev_quarter(), c(2019, 4))
  expect_equal(prev_quarter(), c(2020, 1))
  expect_equal(prev_quarter(), c(2020, 2))
  expect_equal(prev_quarter(), c(2020, 3))
  mockery::expect_called(sys_date_mock, 4)
})