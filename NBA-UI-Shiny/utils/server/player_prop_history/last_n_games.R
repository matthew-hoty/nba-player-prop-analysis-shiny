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