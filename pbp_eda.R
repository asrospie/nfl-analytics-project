library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(nflreadr)
options(scipen = 9999)

head(pbp_99)
colnames(pbp_99)

game_state <- c(
    "ydstogo",
    "yardline_100",
    "posteam_type",
    "qtr",
    "quarter_seconds_remaining",
    "game_seconds_remaining",
    "series_success",
    "goal_to_go",
    "wind",
    "fourth_down",
    "roof",
    "epa",
    "score_differential",
    "field_goal_result",        # Y1
    "fourth_down_converted"     # Y2
)

pbp_99_4_down <- pbp_99 %>%
    filter(down == 4)
nrow(pbp_99_4_down)
sum_na(pbp_99$roof)

sum_na <- function(col) {
    return(sum(is.na(col)))
}

fourth_down_success <- function() {
    return()
}

field_goal_success <- function() {
    return()
}

play_predictor <- function() {
    fg <- field_goal_success()
    fd <- fourth_down_success()

    if (fg > fd && fg > .5) {
        return("field goal")
    } else if (fd > fg && fd > .5) {
        return("go for it")
    } else {
        return("punt that shit")
    }
}

nrow(pbp_99 %>%
    filter(play_type == "punt") %>%
    filter(down == 4))

pbp_21_fg <- pbp_21 %>%
    filter(play_type == "field_goal")
pbp_99_fg <- pbp_99 %>%
    filter(play_type == "field_goal")
max(pbp_21_fg$kick_distance)
max(pbp_99_fg$kick_distance)

pbp_21 <- read.csv("nfl-analytics-project/2021.csv")
