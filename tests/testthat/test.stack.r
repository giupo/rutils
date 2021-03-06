context("Stack")

test_that("I can create a Stack", {
  x <- Stack$new()
  expect_true(is.Stack(x))
  expect_true(inherits(x, "Stack"))
})

test_that("I can push an item into a Stack", {
  x <- Stack$new()
  x$push(1)
  expect_true(!x$empty())
})


test_that("I can peek items from a Stack", {
  x <- Stack$new()
  ex <- 5
  for(i in seq(ex)) {
    x$push(i)
  }
  expect_equal(x$length(), ex)
  expect_equal(x$peek(), ex)
  expect_equal(x$length(), ex)
})

test_that("I can pop (peek and remove) an item from Stack", {
  x <- Stack$new()
  ex <- 5
  for(i in seq(ex)) {
    x$push(i)
  }

  expect_equal(x$length(), ex)
  expect_equal(x$pop(), ex)
  expect_equal(x$length(), ex - 1)
})

test_that("I have error if i peek from an empy Stack", {
  x <- Stack$new()
  expect_error(x$peek())
})

test_that("I have error if I pop from an empty Stack", {
  x <- Stack$new()
  expect_error(x$pop())
})
