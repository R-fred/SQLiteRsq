obj <- NewSQLiteConnection(path = "tests/test_db/chinook.db")
library(RSQLite)
db_conn <- dbConnect(drv = SQLite(), "tests/test_db/chinook.db")

# Get query results with microbenchmark ----
# Results differ wildly from system.time.
# Almost no difference between expressions
# Results in the ns region -> not realistic.
library(rbenchmark)

bm <- benchmark(
system2 = {system2(command = "/usr/bin/sqlite3", args = "/home/fred/Apps/R/resqlite/tests/test_db/chinook.db '.headers on' 'SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;'", stdout = T)},
executeStatement = {ExecuteStatement(obj, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;")},
getqueryresults = {GetQueryResults(obj, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T)},
getqueryresults_old = {GetQueryResults_old(obj, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T)},
rsqlite = {RSQLite::dbGetQuery(db_conn, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;")},
order = 'user.self'
  )


ggplot2::autoplot(mbm)

# Get query results with system.time ----
# Clear advantage to the fread variant
# With fread
time_new_user <- vector(mode = "numeric", length = 100)
time_new_elapsed <- vector(mode = "numeric", length = 100)

for (ii in 1:100) {
  if (ii %% 20 == 0) print(ii)
  timing <- system.time(GetQueryResults(obj, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T))

  time_new_user[ii] <- timing[1]
  time_new_elapsed[ii] <- timing[3]

}
# No fread - own functions
time_old_user <- vector(mode = "numeric", length = 100)
time_old_elapsed <- vector(mode = "numeric", length = 100)

for (ii in 1:100) {
  if (ii %% 20 == 0) print(ii)
  timing <- system.time(GetQueryResults_old(obj, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T))

  time_old_user[ii] <- timing[1]
  time_old_elapsed[ii] <- timing[3]

}

# RSQLite
time_rsqlite_user <- vector(mode = "numeric", length = 100)
time_rsqlite_elapsed <- vector(mode = "numeric", length = 100)

for (ii in 1:100) {
  if (ii %% 20 == 0) print(ii)
  timing <-system.time(dbGetQuery(db_conn, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;"))

  time_rsqlite_user[ii] <- timing[1]
  time_rsqlite_elapsed[ii] <- timing[3]

}

# system2 - reference
time_sys2_user <- vector(mode = "numeric", length = 100)
time_sys2_elapsed <- vector(mode = "numeric", length = 100)

for (ii in 1:100) {
  if (ii %% 20 == 0) print(ii)

  timing <- system.time(fread(text = system2(command = "/usr/bin/sqlite3", args = "/home/fred/Apps/R/resqlite/tests/test_db/chinook.db '.headers on' 'SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;'", stdout = T)))

  time_sys2_user[ii] <- timing[1]
  time_sys2_elapsed[ii] <- timing[3]

}

msg <- paste0("\nTimigs for 'user':",
              "\n\tRSQLite: ", round(mean(time_rsqlite_user) * 1000, 0), " ms",
              "\n\tOld implementation: ", round(mean(time_old_user) * 1000, 0), " ms",
              "\n\tNew implementation: ", round(mean(time_new_user) * 1000, 0), " ms",
              "\n\tSystem2: ", round(mean(time_sys2_user) * 1000, 0), " ms"
)

msg2 <- paste0("\nTimigs for 'elapsed':",
               "\n\tRSQLite: ", round(mean(time_rsqlite_elapsed) * 1000, 0), " ms",
               "\n\tOld implementation: ", round(mean(time_old_elapsed) * 1000, 0), " ms",
               "\n\tNew implementation: ", round(mean(time_new_elapsed) * 1000, 0), " ms",
               "\n\tSystem2: ", round(mean(time_sys2_user) * 1000, 0), " ms"
)

cat(msg)
cat(msg2)

all(res == res_new)

str(res_new)
