source('utils/server/player_prop_history/clean_data.R')

#### Player Props ####
.add_wagerType <- function(df){
  new_df <- df %>%
    mutate(wagerType = case_when(
      marketName %in% c('Moneyline','Spread Betting','Total Points Scored') ~ 'Team',
      tolower(marketType) %like any% c('%team%','%margin%','%line%','%match%',
                                       '%race to%','%wire to wire%','alternate total points%','alternate handicaps%',
                                       'will there be overtime?%',
                                       '%1st%','%2nd%','%3rd%','%4th%','%first%','%second%','%third%','%fourth%') ~ 'Team',
      tolower(marketType) %like any% c('%player%','%to score x%','%to record x%','%made x threes%','%to record a triple double%') ~ 'Player'
    ))
  return(new_df)
}

.filter_to_playerProps <- function(df){
  new_df <- df %>%
    .add_wagerType() %>%
    filter(
      wagerType == 'Player' & 
        marketName != 'Player Performance Doubles' 
    )
  
  return(new_df)
}

get_playerProps <- function(df){
  
  player_props <- df %>%
    .filter_to_playerProps() %>%
    clean_wagerData()
  return(player_props)
}