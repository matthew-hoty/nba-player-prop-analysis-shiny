library(stringr)
#### Evaluate Success ####
#df <- player_prop_hist_df

evaluate_success <- function(df){
  new_df <- df %>%
    .define_target() %>%
    mutate(
      success = ifelse(hadvalue=='OVER', 
                       ifelse(target >= currentHandicap, 1, 0),
                       ifelse(target <= currentHandicap, 1, 0)
      ),
      success_weight = .calc_recency_adj(prevGameNumber) * success
    ) %>%
    .filter_nas()
  return(new_df)
}


.filter_nas <- function(df){
  new_df <- df %>%
    filter(!is.na(currentHandicap)) %>%
    filter(!is.na(success)) 
  return(new_df)
}

.define_target <- function(df){
  new_df <- df %>%
    mutate(
      target = case_when(
        marketType %like any% c('To Score X Points%',
                                'Player X Total Points O/U%',
                                'Player X Alt Total Points%',
                                'PLAYER_.*_TOTAL_POINTS'
                                ) ~ pts, 
        
        marketType %like any% c('To Record X Assists%', 
                                "Player X Total Assists O/U%",
                                'Player X Alt Total Assists%') ~ ast,
        
        marketType %like any% c('To Record X Rebounds%',
                                'Player X Total Rebounds O/U%',
                                'Player X Alt Total Rebounds') ~ treb,
        
        marketType %like any% c('Made X Threes%',
                                'Player X Total 3 Point Field Goals O/U%',
                                'Player X Alt Total Threes%') ~ fg3m,
        
        marketType %like any% c('Player X Total Rebounds + Assists O/U%',
                                'Player X Alt Total Reb + Ast%') ~ ast + treb,
        
        marketType %like any% c('Player X Total Points + Assists O/U%',
                                'Player X Alt Total Pts + Ast%') ~ ast + pts,
        
        marketType %like any% c('Player X Total Points + Rebounds O/U%',
                                'Player X Alt Total Pts + Reb%') ~ treb + pts,
        
        marketType %like any% c('Player X Total Points + Rebounds + Assists O/U%',
                                'Player X Alt Total Pts + Reb + Ast%') ~ ast + treb + pts,
        
        #TRUE ~ 0
      )
    ) 
  return(new_df)
}

.calc_recency_adj <- function(gameNum){
  sigmoid = function(x) {
    1 / (1 + exp(x-9.75))
  }
  return(sigmoid(gameNum))
}

