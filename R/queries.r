# Main functions ----

create_query <- function(qry_type, table, object, values){

  # qry_type: defines if INSERT, DELETE, UPDATE, UPSERT, SELECT
  # based on that, call the proper function to put query together.
  #
  # table: table to execute the query against
  #
  # object: optional, the object to use, can be a dataframe
  #
  # values: single values to enter in database
  #

}

# Support functions ----

chk_tbl_exists <- function(conn, tbl){

# OBSOLTE NOW THAT THE TABLES ARE +

  qry <- sprintf("SELECT name FROM sqlite_master WHERE type='table' AND name='%s';", tbl)




}

chk_tbl_headers <- function(conn, tbl){

  cmd <- paste0(conn@conn_string, " ","\"PRAGMA table_info(", tbl,")\"")

  res <- system(command = cmd, intern = T)
  res <- strsplit(x = res, split = "\\|")
  res <- rapply(res, function(x) x[2])

  return(res)

}

chk_tbls <- function(conn){

  cmd <- paste0(conn@conn_string, " ", ".tables")

  res <- system(command = cmd, intern = T)
  res <- strsplit(x = res, split = "\\s+")
  res <- rapply(res, function(x) x)

  names_res <- res

  res <- as.list(res)
  names(res) <- names_res

  return(res)

}

qry_delete <- function(tbl, ...){

}

qry_insert <- function(tbl, type = c("values", "select", "default"), values){

  qry_vals <- values

  qry <- "\"INSERT INTO tbl VALUES('ID1', 'value1', 'value2')\";"

  # Several inserts in a row can be done by separating the INSERT statements with a space.
  # Count the number of records before and an after insertion. Can be done by using "SELECT COUNT(ROWID) FROM tbl;"

}

qry_insert_values <- function(){

}

qry_insert_select <- function(){

}

qry_insert_default <- function(){

}

qry_insert_file <- function(file_name){

  qry <- "insert into tbl VALUES('ID3', 'value4', readfile('C:/Users/fches/Pictures/Wilfried/_DSC5527.ARW'));"

}

qry_select <- function(tbl, col){

}

qry_select_file <- function(selection_qry){

  qry <- "select writefile('C:/Users/fches/DSC5527.ARW', value_2) FROM tbl WHERE ID='ID3';"

}

qry_update <- function(tbl, ...){

}

qry_upsert <- function(tbl, ...){

}

qry_where <- function(field, value, comparison = c(">", "<", "=", "LIKE")){

  #check for type of value.
  #Only accept, character or numeric
  #value inserted depending on type.

  qry <- paste0("WHERE ", field, "=", "'",value, "'")
  qry <- paste0("WHERE ", field, "=", value)

}