#The sorting should be based on a probability + expected return!! 
# I think we can use odds to extract a 'probability' and then can compare with our model and balance that against expected return (for things like ML or crazy low odds)
library(stringr)
library(DescTools)
#library(RCurl)
library(dplyr)
#library(nbastatR)
#library(ggrepel)
library(lubridate)

source('utils/server/get_data.r')
source('utils/server/wager_prob_funcs.R')
source('utils/server/find_today_opp.R')
source('utils/server/calc_prop_edge.R')
source('utils/server/player_prop_history/player_prop_history.R')

options(scipen=999)

#sort(unique(wagers_df$marketType))
#### UDF #########################################

#wagers_df = today_data$wagers_df; game_logs_df = today_data$game_logs_df; schedule_df = today_data$schedule_df; player_df = today_data$player_df; last_n_games = 10

### Final ####
pred_wager_edge_df <- function(){
  
  today_data <- get_todays_data()
  player_prop_hist_df <- get_playerProp_history(
    wagers_df = today_data$wagers_df, 
    game_logs_df = today_data$game_logs_df,
    schedule_df = today_data$schedule_df,
    player_df = today_data$player_df,
    last_n_games = 10)
  
  edge_df <- calculate_edge(player_prop_hist_df)
  
  player_schedule_df <- find_today_opponent(player_prop_hist_df, today_data$schedule_df, today_data$player_df)
  
  return(list(
    edge_df = edge_df,
    player_prop_hist_df = player_prop_hist_df,
    schedule_df = today_data$schedule_df,
    player_schedule_df = player_schedule_df
    )
  )
}


