
# Startup file

# load required packages
require(RMySQL)
require(ggplot2)
require(ggthemes)
require(plyr)
require(dplyr)
require(twitteR)

# clear global environment
# rm(list=ls(all=TRUE))

# don't want scientific notation
options(scipen = 999)

# close all MySQL connections
for (con in dbListConnections(MySQL())) {dbDisconnect(con)}

# connect to arcturus_demo database
db <- dbConnect(
  MySQL(),
  user='bwilliams_ro',
  password='pR3st!bRspej5zUb',
  dbname='arcturus',
  host='10.10.20.138',
  port=3306
)

# create shorthand query function
query <- function(...) dbGetQuery(db, ...)

# attach arcturus_demo schema
query("use arcturus;")