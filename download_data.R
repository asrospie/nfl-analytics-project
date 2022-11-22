library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(nflreadr)
options(scipen = 9999)
library(DBI)

years <- 1999:2021
for (y in years) {
    temp_df <- load_pbp(y)
    # pbp_df <- rbind(pbp_df, temp_df)
    write.csv(temp_df, paste0(y, ".csv"), row.names = FALSE)
}


db <- dbConnect(RSQLite::SQLite(), "nfl-analytics-project/pbp.sqlite")
for (y in years) {
    temp_df <- read.csv(paste0("nfl-analytics-project/", y, ".csv"))
    table_string <- paste0("pbp_", y)
    dbWriteTable(db, table_string, temp_df)
    print(y)
}
dbDisconnect(db)