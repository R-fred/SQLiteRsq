context("classes")

test_that("Create class", {
  obj <- new(Class = "SQLiteConn")
  expect_equal(object = class(obj)[[1]], expected = "SQLiteConn")
})

test_that("Create class", {
  obj <- SQLConnect(path = ":memory:")
  expect_equal(object = class(obj)[[1]], expected = "SQLiteConn")
  expect_true(is.character(obj@binary))
  expect_true(is.character(obj@db_path))
})