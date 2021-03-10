#The sorting should be based on a probability + expected return!! 
# I think we can use odds to extract a 'probability' and then can compare with our model and balance that against expected return (for things like ML or crazy low odds)
library(stringr)
library(DescTools)
#library(RCurl)
library(dplyr)
#library(nbastatR)
#library(ggrepel)
library(lubridate)

source('utils/get_data.r')
source('utils/wager_prob_funcs.R')

options(scipen=999)


#### UDF #########################################

#### Player Props ####
add_wagerType <- function(df){
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

filter_to_playerProps <- function(df){
  new_df <- df %>%
    add_wagerType() %>%
    filter(
      wagerType == 'Player' & 
      marketName != 'Player Performance Doubles' 
    )
  
  return(new_df)
}

get_playerProps <- function(df){
  
  player_props <- df %>%
    filter_to_playerProps() %>%
    clean_wagerData()
  return(player_props)
}

#### Clean Wager Data ####
clean_wagerData_handicap <- function(df){
  new_df <- df %>% 
    mutate(
      currentHandicap = case_when(
        !is.na(currentHandicap) ~ currentHandicap,
        !is.na(str_extract(marketName,'[0-9]+')) ~ as.numeric(str_extract(marketName,'[0-9]+')),
        !is.na(str_extract(selectionName,'[0-9]+')) ~ as.numeric(str_extract(selectionName,'[0-9]+'))
      )
    )
  return(new_df)
}

clean_wagerData_hadValue <- function(df){
  new_df <- df %>%
    mutate(
      hadvalue = case_when(
        hadvalue != '' & !is.na(hadvalue) ~ hadvalue,
        tolower(selectionName) %like% '%over%' ~ 'O',
        tolower(selectionName) %like% '%under%' ~ 'U',
        !is.na(str_extract(marketName,'\\+')) ~ 'O',
        TRUE ~ hadvalue
      )
    )
  return(new_df)
}

clean_wagerData_namePlayer <- function(df){
  new_df <- df %>%
    mutate(
      namePlayer = trimws(gsub('Over|Under|[0-9\\.]+','',selectionName))
    )
  return(new_df)
}

clean_wagerData <- function(df){
  new_df <- df %>%
    clean_wagerData_handicap() %>%
    clean_wagerData_hadValue() %>%
    clean_wagerData_namePlayer()
  return(new_df)
    
}

#### Get Last N Games ####

get_last_n_games <- function(df,game_logs_df,n=10){
  
  new_df <- df %>%
    left_join(game_logs_df, by = c('namePlayer' = 'namePlayer')) %>%
    group_by(idPlayer) %>% 
    mutate(prevGameNumber = dense_rank(desc(dateGame))
    ) %>%
    filter(prevGameNumber <= n) %>%
    ungroup()
  return(new_df)
}

#### Evaluate Success ####
filter_nas <- function(df){
  new_df <- df %>%
    filter(!is.na(currentHandicap)) %>%
    filter(!is.na(success)) 
  return(new_df)
}

define_target <- function(df){
  new_df <- df %>%
    mutate(
      target = case_when(
        marketType %like% 'To Score X Points%' ~ pts, 
        marketType %like% 'To Record X Assists%' ~ ast, 
        marketType %like% 'To Record X Rebounds%' ~ treb, 
        marketType %like% 'Made X Threes%' ~ fg3m 
        #TRUE ~ 0
      )
    ) 
  return(new_df)
}

evaluate_success <- function(df){
  new_df <- df %>%
    define_target() %>%
    mutate(
      success = ifelse(hadvalue=='O', 
                       ifelse(target > currentHandicap, 1, 0),
                       ifelse(target < currentHandicap, 1, 0)
      )
    ) %>%
    filter_nas()
  return(new_df)
}


#### Find Next Opponent ####
#df <- player_prop_hist_df
#schedule_df <- today_data$schedule_df
#players_dict <- df_nba_player_dict
## Not finished yet
find_next_opponent<- function(df, players_dict, schedule_df){
  today_f = format(Sys.Date(), format="%m-%d-%Y")
  today_sched <- schedule_df %>%
    filter(dateGame == '2021-03-04') %>% #filter(dateGame == today_F)
    select(idTeam, locationGame, slugTeamOpp)
  
  today_players <- df %>% 
    select(gameStartDate,namePlayer) %>%
    distinct() %>%
    left_join(select(players_dict, namePlayer, idTeam), 
              by = c('namePlayer' = 'namePlayer')) %>%
    left_join(today_sched,
              by = c('idTeam' = 'idTeam'))
  
  #new_df <- schedule_df %>% 
    
    
}


#### Player Prop History ####
get_playerProp_history <- function(wagers_df, game_logs_df, schedule_df){
  player_prop_hist_df <- wagers_df %>%
    get_playerProps() %>%
    get_last_n_games(game_logs_df,n=10) %>%
    evaluate_success() %>%
    #find_next_opponent() 
    select(gameStartDate, gameDesc, marketName, marketType, namePlayer,
           currentHandicap, value, hadvalue, wagerType,
           dateGame, success, target, pts, treb,ast,fg3m, 
           urlPlayerHeadshot, urlTeamSeasonLogo, slugOppLoc
    )
  
  return(player_prop_hist_df)
}

#### Calculate Edge ####
calculate_edge <- function(df){
  edge_df <- df %>%
    group_by(marketName, namePlayer) %>%
    summarise(
      odds = mean(value),
      est_probs = round(mean(success),2),
      wager_probs = mean(calc_wager_prob(odds)),
      wager_dec = mean(convert_odds_to_dec(odds)),
      wager_payout = mean(calc_wager_payout(odds)),
      est_odds = convert_probs_to_odds(est_probs),
      est_payout = mean(calc_wager_payout(est_odds)),
      wager_ev = (wager_probs*wager_payout),
      est_ev = (est_probs * est_payout),
      pred_upside = wager_ev - est_ev,
      edge = ((est_probs * wager_dec) - 1),
      ev = (wager_payout * est_probs) - (100 * (1-est_probs))
    ) %>%
    arrange(desc(pred_upside)) %>%
    select(`Player` = namePlayer, 
           `Wager` = marketName, 
           `Odds` = odds, 
           `My Prob` = est_probs,
           `House Prob` = wager_probs,
           `My Value` = pred_upside)
    
}

### Final ####
pred_wager_edge_df <- function(){
  
  today_data <- get_todays_data()
  player_prop_hist_df <- get_playerProp_history(
    today_data$wagers_df, 
    today_data$game_logs_df,
    today_data$schedule_df)
  
  edge_df <- calculate_edge(player_prop_hist_df)
  
  return(list(
    edge_df = edge_df,
    player_prop_hist_df = player_prop_hist_df
    )
  )
}



#### OLD ####
clean_data <- function(wagers_df){
  
  #Team vs player wagers
  player_wager_df <- wagers_df %>%
    mutate(wagerType = case_when(
      marketName %in% c('Moneyline','Spread Betting','Total Points Scored') ~ 'Team',
      tolower(marketType) %like any% c('%team%','%margin%','%line%','%match%',
                                       '%race to%','%wire to wire%','alternate total points%','alternate handicaps%',
                                       'will there be overtime?%',
                                       '%1st%','%2nd%','%3rd%','%4th%','%first%','%second%','%third%','%fourth%') ~ 'Team',
      tolower(marketType) %like any% c('%player%','%to score x%','%to record x%','%made x threes%','%to record a triple double%') ~ 'Player'
    )) %>%
    
    ## Need to figure out a way to dynamically calculate success 
    #player_wager_df <- wagers_df %>%
    filter(wagerType == 'Player' & marketName != 'Player Performance Doubles' ) %>%
    #  filter(marketName == 'To Score 20+ Points') %>%
    mutate(currentHandicap = case_when(
      !is.na(currentHandicap) ~ currentHandicap,
      !is.na(str_extract(marketName,'[0-9]+')) ~ as.numeric(str_extract(marketName,'[0-9]+')),
      !is.na(str_extract(selectionName,'[0-9]+')) ~ as.numeric(str_extract(selectionName,'[0-9]+'))
    ),
    hadvalue = case_when(
      hadvalue != '' & !is.na(hadvalue) ~ hadvalue,
      tolower(selectionName) %like% '%over%' ~ 'O',
      tolower(selectionName) %like% '%under%' ~ 'U',
      !is.na(str_extract(marketName,'\\+')) ~ 'O',
      TRUE ~ hadvalue
    ),
    namePlayer = trimws(gsub('Over|Under|[0-9\\.]+','',selectionName))
    ) %>%
    left_join(game_log_df, by = c('namePlayer' = 'namePlayer')) %>%
    group_by(idPlayer) %>% # how do we handle if a team plays a game but this player doesnt? Like an injury...
    #filter(gameStartDate < as.Date(Sys.time())) %>%
    mutate(prevGameNumber = dense_rank(desc(dateGame)),
           logo = paste('https://cdn.nba.com/logos/nba/',idTeam,'/primary/D/logo.svg',sep='')
    ) %>%
    filter(prevGameNumber <= 10) %>%
    ungroup() %>%
    mutate(slugOppLoc = ifelse(locationGame == 'H', paste('v.',slugOpponent),paste('@',slugOpponent))
    ) %>%
    mutate(
      target = case_when(
        marketType %like% 'To Score X Points%' ~ pts, #evaluate_success(hadvalue,currentHandicap, pts),
        marketType %like% 'To Record X Assists%' ~ ast, #evaluate_success(hadvalue,currentHandicap, ast),
        marketType %like% 'To Record X Rebounds%' ~ treb, #evaluate_success(hadvalue,currentHandicap, treb),
        marketType %like% 'Made X Threes%' ~ fg3m #evaluate_success(hadvalue,currentHandicap, fg3m)
        #TRUE ~ 0
      ),
      success = ifelse(hadvalue=='O', 
                       ifelse(target > currentHandicap, 1, 0),
                       ifelse(target < currentHandicap, 1, 0)
      )
    ) %>%
    filter(!is.na(currentHandicap)) %>%
    filter(!is.na(success)) %>%
    select(gameStartDate, gameDesc, marketName, marketType, namePlayer, currentHandicap, value, hadvalue, wagerType,
           dateGame, success, target, pts, treb,ast,fg3m, 
           urlPlayerHeadshot, urlTeamSeasonLogo,logo, slugOppLoc
    )
  
  return(player_wager_df)
  
}
