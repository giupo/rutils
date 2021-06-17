test_that("setOption works with params", {
  opt <- list(
    .a = 1,
    .b = 2,
    .c = 3,
    .d = 4
  )

  on.exit({
    options(.a = NULL)
    options(.b = NULL)
    options(.c = NULL)
    options(.d = NULL)
  })

  for (key in names(opt)) {
    setOption(key, opt[[key]])
  }
  for (key in names(opt)) {
    expect_equal(getOption(key), opt[[key]])
  }
})

test_that("setOption works with NULLS", {
  opt <- list(
    .a = NULL,
    .b = NULL,
    .c = NULL,
    .d = NULL
  )

  on.exit({
    options(.a = NULL)
    options(.b = NULL)
    options(.c = NULL)
    options(.d = NULL)
  })

  for (key in names(opt)) {
    setOption(key, opt[[key]])
  }
  for (key in names(opt)) {
    expect_equal(getOption(key), opt[[key]])
  }
})

test_that("setOption works with charactes", {
  opt <- list(
    .a = "a",
    .b = "b",
    .c = "c",
    .d = "d"
  )

  on.exit({
    options(.a = NULL)
    options(.b = NULL)
    options(.c = NULL)
    options(.d = NULL)
  })

  for (key in names(opt)) {
    setOption(key, opt[[key]])
  }
  for (key in names(opt)) {
    expect_equal(getOption(key), opt[[key]])
  }
})