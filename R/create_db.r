create_database <- function(db_schema_yml, db_name){

  dbc <- RSQLite::dbConnect(drv = RSQLite::SQLite(), db_name)

  # Create SQL queries ----
  db_schema <- get_db_schema(db_schema_yml)
  tbl_stmts <- create_table_statements(db_schema)
  tbl_idx_stmts <- create_indices_statements(db_schema)

  # Run SQL queries ----
  create_tables(tbl_stmt = tbl_stmts, dbconn = dbc)
  create_indices(tbl_idx_stmt = tbl_idx_stmts, dbconn = dbc)

  print(paste0("New database available at: ", db_name))
  return(TRUE)

}

get_db_schema <- function(db_yml){

  db_schema <- config::get(file = db_yml)

  return(db_schema)

}

create_table_statements <- function(db_schema){

  tables <- db_schema$tables

  create_table_stmt <- function(tbl_def){

    tbl_name <- tbl_def$tbl_name
    tbl_cols <- unlist(tbl_def[2:length(tbl_def)])
    qry_cols <- paste(names(tbl_cols), tbl_cols, collapse = ", ", sep = " ")
    qry <- paste0("CREATE TABLE IF NOT EXISTS ", tbl_name, "(", qry_cols, ")", ";")

    return(qry)

  }

  stmts <- lapply(tables, create_table_stmt)

  return(stmts)

}

create_tables <- function(tbl_stmt, dbconn){

  lapply(unlist(tbl_stmt), dbSendQuery, conn = dbconn)

}

create_indices_statements <- function(db_schema){
  # TODO()
}

create_indices <- function(tbl_idx_stmt, dbconn){

}
