#### Get Last N Games ####

get_last_n_games <- function(df,game_logs_df,n=10){
  
  new_df <- df %>%
    dplyr::left_join(game_logs_df, by = c('namePlayer' = 'namePlayer')) %>%
    dplyr::group_by(idPlayer) %>% 
    dplyr::mutate(prevGameNumber = dplyr::dense_rank(desc(dateGame))
    ) %>%
    dplyr::filter(prevGameNumber <= n) %>%
    dplyr::ungroup()
  return(new_df)
}