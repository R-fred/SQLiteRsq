library(testthat)
library(data.table)

test_that("Write in database", {
  obj <- NewSQLiteConnection(path = "tests/test_db/test_write_db.sqlite")

  col_names <- names(datasets::airquality)
  ExecuteStatement(obj, "'CREATE TABLE IF NOT EXISTS airquality ('Ozone', 'Solar_R', 'Wind', 'Temp', 'Month', 'Day')'")
  insert_response <- InsertData(obj, "airquality", datasets::airquality)

  expect_true(length(insert_response) == 0)

})