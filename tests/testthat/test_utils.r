context("utils")

test_that("Get os version", {
  os <- sessionInfo()[[2]]
  expect_true(grepl(pattern = os, x = get_os()))
})

test_that("Get sqlite cli. Linux", {
  os <- "linux"
  sqlite_bin_os <- get_sqlite_cli_binary(use_sys_exe = T)
  sqlite_bin_pkg <- get_sqlite_cli_binary(use_sys_exe = F)
  expect_true(grepl(pattern = "/usr/bin/sqlite3", x = sqlite_bin_os))
  expect_false(grepl(pattern = "/usr/bin/sqlite3", x = sqlite_bin_pkg))
  expect_true(grepl(pattern = "linux/sqlite3", x = sqlite_bin_pkg))
})

test_that("Get sqlite cli. Windows", {
  os <- "windows"
  sqlite_bin_os <- get_sqlite_cli_binary(use_sys_exe = T)
  sqlite_bin_pkg <- get_sqlite_cli_binary(use_sys_exe = F)
  expect_false(grepl(pattern = "/bin/windows/sqlite3.exe", x = sqlite_bin_os))
  expect_true(grepl(pattern = "/bin/windows/sqlite3.exe", x = sqlite_bin_pkg))
})

test_that("convert to input string", {
  library(data.table)
  dt <- data.table(a = "id1", b = "col1", c = "col2")
  dt2 <- data.table(a = c("id1", "id2"), b = c("col1", "col12"), c = c("col2", "col22"))
  character_vector <- c("id1", "col1", "col2")

  char_input <- convert_char_to_input_string(character_vector)
  dt_input <- convert_dt_to_input_string(dt)
  dt2_input <- convert_dt_to_input_string(dt2)

  expect_equal(object = char_input, expected = "VALUES('id1', 'col1', 'col2')")
  expect_equal(object = dt2_input, expected = c("VALUES('id1', 'col1', 'col2')", "VALUES('id2', 'col12', 'col22')"))
    #first character function
  #then df function.
  #test with several df sizes (up to 1 million rows?)
  "..."
})