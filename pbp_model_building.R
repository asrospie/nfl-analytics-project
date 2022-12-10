library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(nflreadr)
library(rpart)
library(tune)
library(modeldata)
library(tidymodels)
library(DBI)
library(dplyr, warn.conflicts = FALSE)
library(caret)
library(ISLR)
library(MASS)
library(xgboost)
options(scipen = 9999)
set.seed(42)

# load data and filter columns
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


df_complete %>%
    filter(field_goal_result == 1 | field_goal_result == 0) %>%
    head(10)

# Create two dataframes for different models
fg_df <- df_complete %>%
    filter(field_goal_attempt == 1)
go_df <- df_complete %>%
    filter(field_goal_attempt == 0 | field_goal_result == NA) %>%
    filter(play_type == "pass" | play_type ==  "run")
cols <- colnames(df_complete)
fg_cols <- cols[cols != "fourth_down_converted"
    & cols != "play_type" & cols != "field_goal_attempt"]
fg_df <- fg_df[fg_cols]
go_cols <- cols[cols != "field_goal_result"
    & cols != "play_type" & cols != "field_goal_attempt"]
go_df <- go_df[go_cols]
fg_df_factor <- fg_df
go_df_factor <- go_df
fg_df_factor$field_goal_result <- as.factor(fg_df$field_goal_result)
go_df_factor$fourth_down_converted <- as.factor(go_df$fourth_down_converted)
nrow(fg_df)
nrow(go_df)

# Data Split - Train :: 80% ;; Test :: 20%
fg_train_set_all <- sample(1:nrow(fg_df), as.integer(nrow(fg_df) * .8))
fg_test_set_all <- setdiff(1:nrow(fg_df), fg_train_set_all)
go_train_set_all <- sample(1:nrow(go_df), as.integer(nrow(go_df) * .8))
go_test_set_all <- setdiff(1:nrow(go_df), go_train_set_all)

fg_train_factor <- fg_df_factor[fg_train_set_all, ]
fg_test_factor <- fg_df[fg_test_set_all, ]
go_train_factor <- go_df_factor[go_train_set_all, ]
go_test_factor <- go_df[go_test_set_all, ]
colnames(fg_train)
sum(is.na(fg_train_factor))
# model_types <- c("knn", "ctree", "glm", "svmLinear",
    # "svmPoly", "svmRadial", "xgbDART", "xgbLinear", "xgbTree")
model_types <- c("svmLinear", "knn", "ctree", "glm")
train_control <- trainControl(method = "cv", number = 5)
get_accuracy <- function(preds, acts) {
    return(sum(preds == acts) / length(acts))
}
# Find best model for Field goals
best_fg_acc <- 0
best_fg_mdl <- NA
for (model in model_types) {
    if (model == "svmLinear") {
        mdl <- train(
            field_goal_result ~ . - field_goal_result,
            data = fg_train_factor,
            method = model,
            prob.model = TRUE
        )
    }
    else {
        mdl <- train(
            field_goal_result ~ . - field_goal_result,
            data = fg_train_factor,
            method = model,
        )
    }
    preds <- predict(mdl, newdata = fg_test_factor)
    print(predict(mdl, newdata = fg_test_factor, type = "prob"))
    acc <- get_accuracy(
        preds,
        fg_test_factor$field_goal_result
    )
    if (acc > best_fg_acc) {
        best_fg_acc <- acc
        best_fg_mdl <- mdl
    }
    print(paste(model, "acc ::", acc))
}
saveRDS(best_fg_mdl, "nfl-analytics-project/fg_model_v2.rds")
# save(best_fg_mdl, file = "nfl-analytics-data/fg_model_v2.RData")

# Find best model for 4th Down Conversions
best_go_acc <- 0
best_go_mdl <- NA
for (model in model_types) {
    if (model == "svmLinear") {
        mdl <- train(
            fourth_down_converted ~ . - fourth_down_converted,
            data = go_train_factor,
            method = model,
            prob.model = TRUE
        )
    } else {
        mdl <- train(
            fourth_down_converted ~ . - fourth_down_converted,
            data = go_train_factor,
            method = model,
        )
    }
    preds <- predict(mdl, newdata = go_test_factor)
    print(predict(mdl, newdata = go_test_factor, type = "prob"))
    acc <- get_accuracy(
        preds,
        go_test_factor$fourth_down_converted
    )
    if (acc > best_go_acc) {
        best_go_acc <- acc
        best_go_mdl <- mdl
    }
    print(paste(model, "acc ::", acc))
}
saveRDS(best_go_mdl, "nfl-analytics-project/go_model_v2.rds")
# save(best_go_mdl, file = "nfl-analytics-data/go_model_v2.RData")


# Classification reports


# Utility functions

choose_cutoff <- function(preds, acts, start = .5) {
    counter <- start
    best_acc <- 0
    best_cutoff <- start
    while (counter >= 0) {
        test_preds <- ifelse(preds > counter, 1, 0)
        test_acc <- sum(test_preds == acts) / length(acts)
        print(test_acc)
        if (test_acc > best_acc) {
            best_acc <- test_acc
            best_cutoff <- counter
        }
        counter <- counter - .01
    }
    return(best_cutoff)
}
dbDisconnect(db)
