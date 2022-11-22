library(nflfastR)
library(dplyr, warn.conflicts = FALSE)

ids <- nflfastR::fast_scraper_schedules(2017:2019) %>%
    filter(game_type == "SB") %>%
    pull(game_id)

pbp <- nflfastR::build_nflfastR_pbp(ids)

colnames(pbp)
