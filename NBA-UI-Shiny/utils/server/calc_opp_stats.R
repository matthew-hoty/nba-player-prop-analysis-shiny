
#### Calculate Opp Team Stats ####
#df <- player_prop_hist_df
#schedule_df <- today_data$schedule_df
#players_dict <- df_nba_player_dict
#player_df <- today_data$player_df
#game_logs_df <- today_data$game_logs_df
calc_opp_stats <- function(game_logs_df, player_df){
  df <- game_logs_df %>%
    left_join(select(player_df, idPlayer, position, height, weight),
              by = c("idPlayer" = 'idPlayer')) %>%
    group_by(slugOpponent, idGame, position) %>%
    summarise(pts = sum(pts),
              ast = sum(ast),
              treb = sum(treb),
              fg3m = sum(fg3m)) %>%
    group_by(slugOpponent, position) %>%
    summarise(games = n_distinct(idGame),
              pts = sum(pts) / games,
              ast = sum(ast) / games,
              treb = sum(treb) / games,
              fg3m = sum(fg3m) / games
    )
}
