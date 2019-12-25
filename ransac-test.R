library(testthat)

context("testing ransac")


test_that("works", {
  expect_equivalent(
    simulate(reps = 10, seed = 20141028, data = testdata)
    simulate_fast_QR(reps = 10, seed = 20141028, data = testdata)
  )
})