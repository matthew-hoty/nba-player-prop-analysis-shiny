#### Find Next Opponent ####

find_today_opponent<- function(df, schedule_df, player_df){
  ## THIS IS BAD.  I AM RELYING ON A GLOBAL VAR AS A FUNCTION INPUT
  players_dict <- player_df#df_nba_player_dict
  ########################################3
  
  today_f = format(Sys.Date(), format="%Y-%m-%d")
  today_sched <- schedule_df %>%
    filter(dateGame == today_f) %>%
    select(idTeam, locationGame, slugTeamOpp)
  
  today_player_sched <- df %>% 
    select(gameStartDate,namePlayer) %>%
    distinct() %>%
    left_join(select(players_dict, namePlayer, idTeam), 
              by = c('namePlayer' = 'namePlayer')) %>%
    left_join(today_sched,
              by = c('idTeam' = 'idTeam'))
  
  return(today_player_sched)
}