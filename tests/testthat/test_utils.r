context("utils")

test_that("Get os version", {
  os <- sessionInfo()[[2]]
  expect_true(grepl(pattern = get_os(), x = os))
})

test_that("Get proper sqlite binary", {
  expect_true(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})