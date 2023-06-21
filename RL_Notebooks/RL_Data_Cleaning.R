## All the data cleaning stuff to prepare for modeling:

# Load in any necessary libraries for tidying:
library(tidyverse)
library(dplyr)


# read in the team dataset
rl_df <- read.csv("~/Desktop/SLU_Fellows/RL_Datasets/games_by_teams.csv")

# Create a dataset with all of the diff variables
team_diff <- rl_df |> pivot_wider(id_cols = game_id, 
                                  names_from = color,
                                  values_from = -c(1, 2)) |>
  ## all diffs are blue team minus orange team
  mutate(across(where(is.numeric) & ends_with("_blue"),
                .names = "{col}_diff") -
           across(where(is.numeric) & ends_with("_orange"))) |>
  mutate(winner_numeric = if_else(winner_blue == "True",
                                 true = 1,
                                 false = 0)) |> ## winner is 1 for blue, 
                                                ## 0 for orange
  select(game_id, team_id_blue, team_id_orange, winner_numeric, 
         ends_with("_diff"), everything())

# Removing "blue" and "orange" from all of the variable names:
old_names <- names(team_diff)
new_names <- str_remove(old_names, pattern = "_blue") 
names(team_diff) <- new_names

# Remove some of the unnecessary variables to tidy the dataset:
team_diff <- team_diff |> select(-c(59:154))

# Remove variables that are identical to another variable or a linear 
# combination of other variables in the dataset (and goal_diff):
# IMPORTANT NOTE: leave goal_diff in team_diff dataset 
                # but will remove from RL_numeric
team_diff <- team_diff |> select(-c("boost_amount_collected_diff",
                                    "boost_amount_stolen_diff",
                                    "demo_taken_diff",
                                    "winner_orange"))

# create a winner_factor variable that can be used for classification problems
team_diff <- team_diff |> mutate(winner_factor = 
                                   as.factor(as.character(winner_numeric)))

# only keep rows where shooting percentage difference is less than absolute 
# value of 100, because it is not possible for anything to be outside of this
team_diff <- team_diff |> filter(core_shooting_percentage_diff >= -100)
team_diff <- team_diff |> filter(core_shooting_percentage_diff <= 100)

# Remove any NA values:
team_diff <- na.omit(team_diff)

# To make train/test split more fair, keep all series in one or the other,
# not both, by creating a series_id
team_diff <- team_diff |> 
  mutate(series_id = paste(team_id, team_id_orange, sep = "_"))

# reorder the dataset so all of the non-numeric variables come before numeric:
team_diff <- team_diff |>
  select(series_id, game_id, team_id, team_id_orange, winner, winner_factor, 
         winner_numeric, team_slug, team_slug_orange, team_name, 
         team_name_orange, team_region, team_region_orange, ends_with("_diff"), 
         everything())

# Output the csv file for team_diff dataset:
# write_csv(x = team_diff, "~/Desktop/SLU_Fellows/RL_Datasets/team_diff.csv")





# Load in the players dataset:
games_by_players <- 
  read_csv("~/Desktop/SLU_Fellows/RL_Datasets/games_by_players.csv")

#Calculate the team's standard deviations within each variable within each game:
team_std <- games_by_players |>
  group_by(game_id, team_id) |>
  # calculate the standard deviations for each team for each numeric variable
  summarise(across(where(is.numeric), \(x) sd(x,
                                              na.rm =TRUE),
                   .names = "sd_{.col}"),
            color = sample(color, size = 1),
            winner = sample(winner, size = 1)) |>
  ungroup() 


team_std <- team_std |> select(game_id, color, everything()) |>
  pivot_wider(id_cols = game_id, 
              names_from = color,
              values_from = -c(1, 2)) |>
  # keep only game_id, team_id, and SDs 
  select(game_id, winner_blue, starts_with("sd_")) 

team_std <- team_std |> 
  mutate(across(where(is.numeric) & ends_with("_blue"),
                .names = "{col}_diff") -
           across(where(is.numeric) & ends_with("_orange"))) |>
  # keep only diff variables, and game_id
  select(game_id, winner_blue, ends_with("_diff")) 


# Merge this dataset with team_diff:
RL_joined <- left_join(team_diff, team_std, by = c("game_id" = "game_id"))

# Remove rows with NAs from the dataset
RL_joined <- na.omit(RL_joined)

# Remove sd diff variables that are not in dataset as regular diff predictors
# Also remove any predictors that don't make sense (like camera stats)
# (also remove regular positioning_time_in_front_ball_diff because 
# already have time behind)
RL_joined <- RL_joined %>% select(-c("sd_core_goals_blue_diff",
                                     "sd_core_assists_blue_diff",
                                     "sd_core_score_blue_diff",
                                     "sd_boost_amount_collected_blue_diff",
                                     "sd_boost_amount_stolen_blue_diff",
                                     starts_with("sd_boost_percent"),
                      "sd_movement_avg_powerslide_duration_blue_diff",
                      "sd_movement_avg_speed_percentage_blue_diff",
                      starts_with("sd_movement_percent"),
                      "positioning_time_in_front_ball_diff",
                      "sd_positioning_time_in_front_ball_blue_diff",
                  "sd_positioning_goals_against_while_last_defender_blue_diff",
                  "sd_positioning_time_closest_to_ball_blue_diff",
                  "sd_positioning_time_farthest_from_ball_blue_diff",
                      starts_with("sd_positioning_percent"),
                      "sd_demo_taken_blue_diff",
                      starts_with("sd_advanced"),
                      "sd_car_id_blue_diff",
                      "sd_steering_sensitivity_blue_diff",
                      starts_with("sd_camera"),
                  "sd_core_shooting_percentage_blue_diff",
                  "sd_movement_avg_speed_blue_diff",
                  "sd_positioning_time_most_back_blue_diff",
                  "sd_positioning_time_most_forward_blue_diff",
                  starts_with("sd_positioning_avg")))


# Output a csv with the joined team and players datasets:
# (this should include all of the diff variables and all of the sd variables)
write_csv(x = RL_joined, "~/Desktop/SLU_Fellows/RL_Datasets/RL_joined.csv")






## NOW CREATE AND TIDY RL_numeric AS SEPARATE CSV:

# Only keep winner and variables that are numeric:
RL_numeric <- RL_joined %>% select(where(is.numeric), series_id, winner_factor)

# Remove some numeric predictors that have obvious correlation to game outcome:
RL_numeric <- RL_numeric %>% select(-c("core_goals_diff",
                                       "core_assists_diff",
                                       # keeping winnner_factor instead 
                                       # of winner numeric
                                       "winner_numeric",
                                       "core_score_diff",
                                       "core_shooting_percentage_diff"))

# Order the dataset to have series_id and winner factor come first
RL_numeric <- RL_numeric %>% select(series_id, winner_factor, everything())

# Create a csv file for RL_numeric dataset:
write_csv(x = RL_numeric, "~/Desktop/SLU_Fellows/RL_Datasets/RL_numeric.csv")



