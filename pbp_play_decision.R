library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(nflreadr)
library(DBI)
options(scipen = 9999)
set.seed(42)

get_test_data <- function() {
    db <- dbConnect(RSQLite::SQLite(), "nfl-analytics-project/pbp.sqlite")
    query_str <- "select ydstogo, yardline_100, posteam_type, qtr, 
        quarter_seconds_remaining, game_seconds_remaining, 
        series_success, goal_to_go, wind, down, roof, 
        epa, score_differential, field_goal_attempt, field_goal_result, 
        play_type, fourth_down_converted 
        from pbp_all where down = '4'"
    df <- dbGetQuery(db, query_str)
    na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
    print(data.frame(na_count))

    nrow(df)
    # remove all rows with any missing data
    df_complete <- df %>%
        filter(!is.na(roof)) %>%
        filter(!is.na(epa)) %>%
        filter(!is.na(field_goal_result) | !is.na(fourth_down_converted)) %>%
        filter(!is.na(play_type))
    df_complete %>% head(10)
    df_complete$wind[is.na(df_complete$wind)] <- 0
    nrow(df_complete)

    # Transform data
    df_complete$field_goal_result <-
        ifelse(df_complete$field_goal_result == "made", 1, 0)
    df_complete$posteam_type <-
        ifelse(df_complete$posteam_type == "home", 1, 0)
    df_complete$roof <-
        ifelse(df_complete$roof == "dome" | df_complete$roof == "closed", 0, 1)
    dbDisconnect(db)
    train_sample <- sample(1:nrow(df_complete),
        as.integer(nrow(df_complete) * .8))
    test_sample <- setdiff(1:nrow(df_complete), train_sample)

    return(df_complete[test_sample, ])
}

field_goal_success_rate <- function(df) {
    y <- df[c("ydstogo", "yardline_100", "posteam_type", "qtr",
        "quarter_seconds_remaining", "game_seconds_remaining",
        "series_success", "goal_to_go", "wind", "down", "roof",
        "epa", "score_differential")]
    mdl <- readRDS("nfl-analytics-project/fg_model_v2.rds")
    return(predict(mdl, newdata = y, type = "prob"))
}

fourth_down_success_rate <- function(df) {
    y <- df[c("ydstogo", "yardline_100", "posteam_type", "qtr",
        "quarter_seconds_remaining", "game_seconds_remaining",
        "series_success", "goal_to_go", "wind", "down", "roof",
        "epa", "score_differential")]
    mdl <- readRDS("nfl-analytics-project/go_model_v2.rds")
    return(predict(mdl, newdata = y, type = "prob"))
}

fourth_down_play_call <- function(df) {
    fg_sr <- field_goal_success_rate(df)
    fd_sr <- fourth_down_success_rate(df)

    decisions <- c()
    for (i in seq_len(nrow(df))) {
        if (fg_sr[i, ][2] < .5 & fd_sr[i, ][2] < .5) {
            decisions <- append(decisions, "punt")
        } else if (fg_sr[i, ][2] > fd_sr[i, ][2]) {
            decisions <- append(decisions, "field goal")
        } else {
            decisions <- append(decisions, "go for it")
        }
    }
    return(decisions)
}

df <- get_test_data()

play_calls <- fourth_down_play_call(df)

count_decisions <- function(l) {
    field_goals <- 0
    go_for_it <- 0
    punts <- 0
    for (i in seq_len(length(l))) {
        if (l[i] == "field goal") {
            field_goals <- field_goals + 1
        } else if (l[i] == "go for it") {
            go_for_it <- go_for_it + 1
        } else {
            punts <- punts + 1
        }
    }
    return(c(field_goals, go_for_it, punts))
}

print(count_decisions(play_calls))