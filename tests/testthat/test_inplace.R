
test_that("%+=% operator works correctly", {
  x <- 5
  x %+=% 3
  expect_equal(x, 8)

  y <- c(1, 2, 3)
  y %+=% 1
  expect_equal(y, c(2, 3, 4))
})

test_that("%-=% operator works correctly", {
  x <- 10
  x %-=% 4
  expect_equal(x, 6)

  y <- c(5, 4, 3)
  y %-=% 1
  expect_equal(y, c(4, 3, 2))
})

test_that("%c=% operator works correctly", {
  x <- c(1, 2, 3)
  x %c=% c(4, 5)
  expect_equal(x, c(1, 2, 3, 4, 5))

  y <- "hello"
  y %c=% " world"
  expect_equal(y, c("hello", " world"))
})

test_that("%union=% operator works correctly", {
  x <- c(1, 2, 3)
  x %union=% c(3, 4, 5)
  expect_equal(x, c(1, 2, 3, 4, 5))

  y <- c("a", "b", "c")
  y %union=% c("c", "d", "e")
  expect_equal(y, c("a", "b", "c", "d", "e"))
})

test_that("assignPlus function works correctly", {
  x <- 5
  assignPlus("x", 3)
  expect_equal(x, 8)

  y <- c(1, 2, 3)
  assignPlus("y", 1)
  expect_equal(y, c(2, 3, 4))
})

test_that("assignMinus function works correctly", {
  x <- 10
  assignMinus("x", 4)
  expect_equal(x, 6)

  y <- c(5, 4, 3)
  assignMinus("y", 1)
  expect_equal(y, c(4, 3, 2))
})

test_that("assignC function works correctly", {
  x <- c(1, 2, 3)
  assignC("x", c(4, 5))
  expect_equal(x, c(1, 2, 3, 4, 5))

  y <- "hello"
  assignC("y", " world")
  expect_equal(y, c("hello", " world"))
})

test_that("assignUnion function works correctly", {
  x <- c(1, 2, 3)
  assignUnion("x", c(3, 4, 5))
  expect_equal(x, c(1, 2, 3, 4, 5))

  y <- c("a", "b", "c")
  assignUnion("y", c("c", "d", "e"))
  expect_equal(y, c("a", "b", "c", "d", "e"))
})


test_that("%setdiff=% operator works correctly", {
  x <- c(1, 2, 3, 4, 5)
  x %setdiff=% c(3, 4)
  expect_equal(x, c(1, 2, 5))

  y <- c("a", "b", "c", "d")
  y %setdiff=% c("b", "d")
  expect_equal(y, c("a", "c"))
})

test_that("assignSetdiff function works correctly", {
  x <- c(1, 2, 3, 4, 5)
  assignSetdiff("x", c(3, 4))
  expect_equal(x, c(1, 2, 5))

  y <- c("a", "b", "c", "d")
  assignSetdiff("y", c("b", "d"))
  expect_equal(y, c("a", "c"))
})