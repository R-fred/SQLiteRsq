# Get query results with microbenchmark ----
# Results differ wildly from system.time.
library(microbenchmark)

mbm <- microbenchmark(
  list = list(
    fread = {res <- GetQueryResults(obj2, "SELECT * FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T, default = T)},
    no_fread = {res <- GetQueryResults_old(obj2, "SELECT * FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T)}
  ),
  times = 1000000L
)

ggplot2::autoplot(mbm)

# Get query results with system.time ----
# Clear advantage to the fread variant
# With fread
system.time(for(ii in 1:50){res_new <-GetQueryResults(obj2, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T, default = T)})
# No fread - own functions
system.time(for(ii in 1:50){res <-GetQueryResults_old(obj2, "SELECT AlbumId, Title, artists.Name FROM albums INNER JOIN artists ON albums.ArtistId = artists.ArtistId;", dataTable = T)})

all(res == res_new)
