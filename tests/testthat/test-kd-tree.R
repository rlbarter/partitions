library(testthat)
context("Tree splits")

kdtree <- kdTree(iris[,-5], max.depth = 5)

test_that("kdtree is the correct dimension", {
  expect_equal(nrow(kdtree), 2^max.depth - 1)
})

test_that("each depth is repeated the correct number of times", {
  expect_equal(sum(kdtree$depth == 2), 2^1)
  expect_equal(sum(kdtree$depth == 3), 2^2)
  expect_equal(sum(kdtree$depth == 4), 2^3)
})

test_that("the largest axis is equal to the number of variables", {
  expect_equal(max(kdtree$axis), ncol(iris[,-5]))
})

test_that("each node completely splits the data", {
  children <- dplyr::filter(kdtree, depth == 2, axis == 2)
  expected.vector <- rep(1, 150)
  names(expected.vector) <- paste0("d", 1:150)
  expect_identical(sapply(children, sum)[4:153], expected.vector)

  children <- dplyr::filter(kdtree, depth == 3, axis == 3)
  expected.vector <- rep(1, 150)
  names(expected.vector) <- paste0("d", 1:150)
  expect_identical(sapply(children, sum)[4:153], expected.vector)
})

test_that("every data point falls in EXACTLY ONE terminal node", {
  terminal.nodes <- dplyr::filter(kdtree, depth == 5)
  expected.vector <- rep(1, 150)
  names(expected.vector) <- paste0("d", 1:150)
  expect_identical(sapply(terminal.nodes, sum)[4:153], expected.vector)
})
