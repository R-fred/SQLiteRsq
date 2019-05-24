context("Database tables")

test_that("Create a new db with a table", {

  if (file.exists("tests/testthat/test_create_db.sqlite")) {
    file.remove("tests/testthat/test_create_db.sqlite")
    }
  system(command = paste0(obj@binary, " ", "tests/testthat/test_create_db.sqlite", " 'CREATE TABLE tbl(id, col1, col2);'"), intern = T)
  system(command = paste0(obj@binary, " ", "tests/testthat/test_create_db.sqlite", " 'INSERT INTO tbl VALUES(\"r1c1\", \"r1c2\", \"r1c3\");'", , " 'INSERT INTO tbl VALUES(\"r2c1\", \"r2c2\", \"r2c3\");'", , " 'INSERT INTO tbl VALUES(\"r3c1\", \"r3c2\", \"r3c3\");'"), intern = T)

  expect_true(file.exists("tests/testthat/test_create_db.sqlite"))

})

test_that("List tables and fields", {
  tbl <- system(command = paste0(obj@binary, " ", "tests/testthat/test_create_db.sqlite", " .tables"), intern = T)
  headers <- system(command = paste0(obj@binary, " ", "tests/testthat/test_create_db.sqlite", " 'PRAGMA table_info(tbl);'"), intern = T)

  expect_equal(object = tbl, expected = "tbl")
  expect_true(length(headers) == 3)
  expect_equal(headers[1], expected = "0|id||0||0")
  expect_equal(headers[2], expected = "1|col1||0||0")
  expect_equal(headers[3], expected = "2|col2||0||0")

})