library(nbastatR)
library(future)
plan(multiprocess) 
library(dplyr)
library(ggplot2)
library(zoo) #rollmean
library(time)
library(caret)
library(e1071)

hist_data = readRDS("boxscores.rds")

bs_list_players = lapply(hist_data,function(x) x$dataBoxScore[1])
bs_list_teams = lapply(hist_data,function(x) x$dataBoxScore[2])

players_df = bind_rows(bs_list_players)
team_df = bind_rows(bs_list_teams)
#############################################################
game_log_df = game_logs(seasons = 2020:2021)

team_df = game_log_df %>%
  select(slugSeason, dateGame, idGame, slugTeam, numberGameTeamSeason, isB2B, isB2BFirst, isB2BSecond, locationGame, countDaysRestTeam) %>%
  distinct() %>%
  right_join(team_df, by=c('idGame','slugTeam'))

# ortg = points per 100 possessions
# drtg = points given up per 100p

df = team_df %>%
  filter(slugTeam == 'CLE')

ggplot(df, aes(x=1:nrow(df), y = possessions)) + 
  geom_point() + geom_line() + 
  geom_line(aes(y=rollmean(possessions, 7, fill = NA,align = 'right')), colour = 'blue', size = 1.5)


##how much does opponents pace impact a teams pace
x = team_df %>%
  arrange(idGame) %>%
  group_by(slugTeam) %>%
  mutate(cummean_poss = cummean(possessions) # season average 
         ) %>%
  select(dateGame, idGame, slugTeam, locationGame,
         numberGameTeamSeason, isB2B, isB2BFirst, isB2BSecond, countDaysRestTeam,
         pts, possessions, cummean_poss) %>%
  left_join(.,., by = "idGame") %>%
  filter(slugTeam.x != slugTeam.y)


#####################################
set.seed(100)
trainRowNumbers <- createDataPartition(x$possessions.x, p=0.8, list=FALSE)
preProcess_model <- preProcess(x, method=c("range"))
newdata <- predict(preProcess_model, newdata = x)
trainData <- newdata[trainRowNumbers,]
testData <- newdata[-trainRowNumbers,]
#idGame, slugTeam.x,
trainData$y = trainData$possessions.x - trainData$cummean_poss.x

model_lm1 = train(possessions.x ~ cummean_poss.x + cummean_poss.y , data=trainData, method='lm')
model_lm2 = train(y ~ cummean_poss.y , data=trainData, method='lm')

model_rf1 = train(possessions.x ~ cummean_poss.x + cummean_poss.y , data=trainData, method='rf')
model_rf2 = train(y ~ cummean_poss.y , data=trainData, method='rf')

model_nnet1 = train(possessions.x ~ cummean_poss.x + cummean_poss.y , data=trainData, method='nnet', maxit = 2000)
model_nnet2 = train(y ~ cummean_poss.y , data=trainData, method='nnet',  maxit = 2000)


## Predict team points possessions based on season avg of both teams
summary(lm(possessions.x ~ cummean_poss.x + cummean_poss.y + 
             locationGame.x + isB2BSecond.x + isB2BSecond.y
           , data = trainData))

summary(lm(possessions.x - cummean_poss.x ~ cummean_poss.y
           , data = trainData))

summary(lm(possessions.x ~ cummean_poss.x + cummean_poss.y
           , data = trainData))
## Predict team points based on season avg possessions for both teams
summary(lm(pts.x ~ cummean_poss.x + cummean_poss.y, data = x))



##################
df = x %>%
  filter(slugTeam == 'CLE') %>%
  select(idGame, slugTeam, possessions, cummean_poss)



ggplot(x, aes(x = cummean_poss.y, y = possessions.x - cummean_poss.x)) + 
  geom_point() 






