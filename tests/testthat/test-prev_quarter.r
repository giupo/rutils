
testthat::test_that("prev_quarter does what it claims", {
  expect_equal(prev_quarter(as.Date("2020-01-01")), c(2019, 4))
  expect_equal(prev_quarter(as.Date("2020-02-15")), c(2019, 4))
  expect_equal(prev_quarter(as.Date("2020-03-31")), c(2019, 4))
  expect_equal(prev_quarter(as.Date("2020-04-01")), c(2020, 1))
  expect_equal(prev_quarter(as.Date("2020-09-01")), c(2020, 2))
  expect_equal(prev_quarter(as.Date("2020-12-01")), c(2020, 3))
})
