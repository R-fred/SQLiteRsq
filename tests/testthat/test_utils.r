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
  df <- data.frame()
  character_vector <- character()
  #first character function
  #then df function.
  #test with several df sizes (up to 1 million rows?)
  "..."
})