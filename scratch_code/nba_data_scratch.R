#library(devtools)
#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(future)
plan(multiprocess) 
library(dplyr)
library(ggplot2)
library(zoo) #rollmean
library(time)

game_log_df = game_logs(seasons = 2020:2021)

#team_game_log = game_logs()

df = game_log_df %>%
  #filter(slugTeam == "CLE") %>%
  filter(namePlayer == 'Andre Drummond')

names(bs_list)
unique(game_log_df$idGame)

game_log_df = game_logs(seasons = 2019)


bs_list = list()

for(i in 1111:length(unique(game_log_df$idGame))){
  print(i)
  id = unique(game_log_df$idGame)[i]
  bs_list[[i]] = box_scores(game_ids = unique(game_log_df$idGame)[i])
  print(i*100/length(unique(game_log_df$idGame)))
  print(Sys.time())
}

saveRDS(bs_list,'boxscores_2018_2019.rds')

bs_list[[4]]$dataBoxScore[1]


bs_list_players = lapply(bs_list,function(x) x$dataBoxScore[1])

bs_list_players = rbind(bs_list_players, data.frame)

bs_list_players[[2]]

box_score_df = box_scores(game_ids = unique(game_log_df$idGame))


ggplot(df, aes(x=1:nrow(df))) + 
  geom_line(aes(y=pts)) + 
  geom_point(aes(y=pts)) +
  geom_line(aes(y=rollmean(pts, 7, fill = NA,align = 'right')), colour = 'blue', size = 1.5) +
  geom_hline(yintercept=14.5, colour = 'red')
  

ggplot(df) + 
  geom_histogram(aes(minutes), binwidth = 5)

mean(df$minutes)

ggplot(df) + 
  geom_point(aes(minutes, pf)) 

summary(lm(minutes~pf, data = df))




ggplot(game_log_df) + 
  

# points for 
# 






