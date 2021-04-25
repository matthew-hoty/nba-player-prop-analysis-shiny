

source('utils/server/player_prop_history/get_playerProps.R')
source('utils/server/player_prop_history/evaluate_success.R')
source('utils/server/player_prop_history/last_n_games.R')

#### Player Prop History ####
get_playerProp_history <- function(wagers_df, game_logs_df, schedule_df, player_df, last_n_games = 10){
  player_prop_hist_df <- wagers_df %>%
    get_playerProps() %>%
    get_last_n_games(game_logs_df,n=last_n_games) %>%
    evaluate_success() %>%
    select(idGame, idTeam, idPlayer, FD_gameId = gameId, FD_marketId = marketId,
           FD_marketTypeId = marketTypeId, FD_selectionId = selectionId, 
           gameStartDate, gameDesc, marketName, marketType, slugTeam,idTeam, namePlayer,
           currentHandicap, value, hadvalue, wagerType, marketDesc,
           dateGame, prevGameNumber, success_weight, success, target, pts, treb,ast,fg3m, 
           urlPlayerHeadshot, urlTeamSeasonLogo, slugOppLoc
    )
  
  return(player_prop_hist_df)
}