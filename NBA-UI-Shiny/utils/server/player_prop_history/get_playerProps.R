source('utils/server/player_prop_history/clean_data.R')

#df = wagers_df

#### Player Props ####
get_playerProps <- function(df){
  
  player_props <- df %>%
    .filter_to_playerProps() %>%
    .filter_to_today() %>%
    clean_wagerData()
  return(player_props)
}

.add_wagerType <- function(df){
  new_df <- df %>%
    dplyr::mutate(
      wagerType = dplyr::case_when(
        marketName %in% c('MONEY_LINE','Spread Betting','Total Points Scored') ~ 'Team',
        marketType %like any% c(
          'PLAYER_.*_TOTAL_POINTS',
          'PLAYER_.*_TOTAL_ASSISTS',
          'PLAYER_.*_TOTAL_MADE_3_POINT_FIELD_GOALS',
          'PLAYER_.*_TOTAL_REBOUNDS',
          
          'TO_SCORE_.*_POINTS',
          'TO_RECORD_.*_ASSISTS',
          'TO_RECORD_.*_REBOUNDS',
          
          '.*_MADE_THREES',
          
          'TO_RECORD_A_.*_DOUBLE',
          'FIRST_BASKET_(SAME_GAME_MULTIS)',
          'PLAYER_PERFORMANCE_DOUBLES'
        ) ~ 'Player'
      )
    )
  return(new_df)
}

.filter_to_playerProps <- function(df){
  new_df <- df %>%
    .add_wagerType() %>%
    dplyr::filter(
      wagerType == 'Player'
    )
  if(nrow(new_df) == 0){
    print('NO PLAYER PROPS BRO')
  }
  
  return(new_df)
}

.filter_to_today <- function(df) {
  new_df <- df %>%
    dplyr::filter(gameStartDate == Sys.Date())
  
  return(new_df)
}
