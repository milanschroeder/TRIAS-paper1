# 00_connectDB.R #####

### establish database connection:

library(DBI)
library(RMariaDB)
library(rstudioapi)

con <- DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "trias",
  username = rstudioapi::askForPassword("username:"),
  password = rstudioapi::askForPassword("password:")
  )