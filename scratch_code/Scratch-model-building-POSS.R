library(nbastatR)
library(future)
plan(multiprocess) 
library(dplyr)
library(ggplot2)
library(zoo) #rollmean
#library(time)
library(caret)
library(e1071)
library(Metrics)
library(tidyverse)


hist_data = readRDS("data/boxscores.rds")

bs_list_players = lapply(hist_data,function(x) x$dataBoxScore[1])
bs_list_teams = lapply(hist_data,function(x) x$dataBoxScore[2])

players_df = bind_rows(bs_list_players)
team_df = bind_rows(bs_list_teams)
#team_df2 <- read.csv('data/team_boxscores_2021.csv')
#########################################################
game_log_df = game_logs(seasons = 2020:2021)

team_df = game_log_df %>%
  select(slugSeason, dateGame, idGame, slugTeam, numberGameTeamSeason, isB2B, isB2BFirst, isB2BSecond, locationGame, countDaysRestTeam) %>%
  distinct() %>%
  right_join(team_df, by=c('idGame','slugTeam'))

##how much does opponents pace impact a teams pace

model_df = team_df %>%
  filter(minExact == 240) %>%
  arrange(idGame) %>%
  group_by(slugTeam) %>%
  mutate(cummean_poss = lag(cummean(possessions),1), # season average entering game
         cummean_tov = lag(cummean(tov),1),
         cummean_blk = lag(cummean(blk),1),
         cummean_stl = lag(cummean(stl),1),
         cummean_oreb = lag(cummean(oreb),1),
         cummean_ortg = lag((cumsum(pts)/cumsum(possessions)) * 100,1),
         cummean_poss_l10 = rollapply(lag(possessions,1),10,mean,align='right',fill='extend'), #rollmean(possessions, 10),
         cummean_poss_l5 = rollapply(lag(possessions,1),5,mean,align='right',fill='extend'), #rollmean(possessions, 5),
         cummean_poss_l3 = rollapply(lag(possessions,1),3,mean,align='right',fill='extend'), #rollmean(possessions, 3)
         restDays = case_when(
           countDaysRestTeam < 5 ~ as.character(countDaysRestTeam),
           countDaysRestTeam >= 5 ~ ">=5",
         ),
  ) %>%
  select(dateGame, idGame, 
         slugTeam, locationGame,
         isB2BFirst, isB2BSecond, restDays,#countDaysRestTeam,
         cummean_poss, cummean_poss_l10,cummean_poss_l5,cummean_poss_l3,
         cummean_tov, 
         cummean_blk,
         cummean_stl,
         cummean_oreb,
         cummean_ortg,pts,
         possessions
        ) %>%
  left_join(.,., by = c("idGame","dateGame"), suffix=c('','.opp')) %>%
  filter(slugTeam != slugTeam.opp) %>%
  select(-possessions.opp, -locationGame.opp) %>%
  ungroup()

model_df_ids <- select(model_df, dateGame, idGame)
model_df <- select(model_df, -dateGame, -idGame)

#######################3
## Possible Predictors
# Season Average possessions 
# Season Average opponents posessions

# back to backs (first v second)
# Days off from last game
# distance from last game

###############################################
set.seed(100)
trainRowNumbers <- createDataPartition(model_df$possessions, p=0.8, list=FALSE)
preProcess_model <- preProcess(model_df[,-which(names(model_df) %in% c('possessions'))], method=c("center", "scale"))
newdata <- predict(preProcess_model, newdata = model_df)
trainData <- newdata[trainRowNumbers,]
testData <- newdata[-trainRowNumbers,]

## 10-fold CV
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

###################################################
## Linear Regression 
model_lm1 = train(possessions ~ cummean_poss + cummean_poss.opp + 
                    slugTeam + slugTeam.opp
                  , data=trainData
                  , method='lm'
                  , trControl = fitControl
                  , na.action=na.omit)

model_lm3 = train(possessions ~ cummean_poss + cummean_poss.opp + 
                    slugTeam + slugTeam.opp + 
                    #cummean_poss_l10 + cummean_poss_l10.opp +
                    #cummean_poss_l5 + cummean_poss_l5.opp +
                    cummean_poss_l3 + cummean_poss_l3.opp + 
                    cummean_tov + cummean_tov.opp +
                    #cummean_blk + cummean_blk.opp +
                    #cummean_stl + cummean_stl.opp +
                    #cummean_oreb + cummean_oreb.opp +
                    restDays + restDays.opp 
                    #isB2BFirst + isB2BSecond + 
                    #isB2BFirst.opp + isB2BSecond.opp
                  , data=trainData
                  , method='lm'
                  , trControl = fitControl
                  , na.action=na.omit)

model_lm4 = train(possessions ~ . 
                    , data=trainData, method='lm',trControl = fitControl,na.action=na.omit)


model_lm1
model_lm3
model_lm4
summary(model_lm1)
summary(model_lm3)
summary(model_lm4)

# collect resamples
results <- resamples(list("1"=model_lm1, "3"=model_lm3, "4"=model_lm4))
# summarize the distributions
summary(results)
bwplot(results)
plot(varImp(model_lm4))

cor(model_df[,-which(names(model_df) %in% c('possessions'))])

cor(model_df[,c('cummean_poss',"cummean_poss_l10","cummean_poss_l5","cummean_poss_l3")])

plot(model_df$cummean_poss,model_df$cummean_poss_l3)

plot(cor(c(model_df$cummean_poss_l10,model_df$cummean_poss_l5)))

results <- rfe()

print("base model rmse:")
rows <- as.integer(names(predict(model_lm3, testData)))
print(rmse(predict(model_lm3, testData), testData$possessions[rows]))

rows <- as.integer(names(predict(model_lm4, testData)))
print(rmse(predict(model_lm4, testData), testData$possessions[rows]))


print("tuned model rmse:")
print(rmse(predict(tuned_model$finalModel, testing), testing$solo_WinRatio))

rows <- as.integer(names(predict(model_lm3, testData)))
plot(testData$possessions[rows], predict(model_lm3, testData))

###################################################################

model_lm4 = train(possessions ~ . 
                  , data=trainData, method='lm',trControl = fitControl,na.action=na.omit)


#########################################################
x <- select(trainData, possessions,
            slugTeam, slugTeam.opp, 
            cummean_poss , cummean_poss.opp , 
            cummean_poss_l10 , cummean_poss_l10.opp ,
            cummean_poss_l5 , cummean_poss_l5.opp ,
            cummean_poss_l3 , cummean_poss_l3.opp , 
            cummean_tov , cummean_tov.opp ,
            restDays , restDays.opp 
) %>%
  cbind(
    data.frame(
      predict(
        dummyVars("~ restDays + restDays.opp + slugTeam + slugTeam.opp", data = .)
        , newdata = .)
    )
  ) %>%
  drop_na() %>%
  select(!c(slugTeam, slugTeam.opp,restDays,restDays.opp))

control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x[,-1], as.matrix(x[,1]),sizes = c(1:length(x[,-1])), rfeControl=control)
print(results)
plot(results)
predictors(results)


###################################################
## GBM

gbm_grid = expand.grid(interaction.depth = c(1, 2, 3),
                       n.trees = (1:30) * 100,
                       shrinkage = c(0.1, 0.3),
                       n.minobsinnode = 20)
model_gbm1 = train(possessions ~ cummean_poss + cummean_poss.opp + 
                    slugTeam + slugTeam.opp + 
                    cummean_poss_l10 + cummean_poss_l10.opp +
                    cummean_poss_l5 + cummean_poss_l5.opp +
                    cummean_poss_l3 + cummean_poss_l3.opp
                   , data=trainData
                   , method='gbm'
                   , trControl = fitControl
                   , tuneGrid = gbm_grid,
                   , na.action=na.pass)
model_gbm1
summary(model_gbm1)
plot(model_gbm1)
#########################################
## Other models

model_rf1 = train(possessions ~ cummean_poss + cummean_poss.opp 
                  , data=trainData, method='rf', trControl = fitControl)
model_rf2 = train(y ~ cummean_poss.opp 
                  , data=trainData, method='rf', trControl = fitControl)

model_nnet1 = train(possessions ~ cummean_poss + cummean_poss.opp 
                    , data=trainData, method='nnet', maxit = 2000, trControl = fitControl)
model_nnet2 = train(y ~ cummean_poss.opp 
                    , data=trainData, method='nnet',  maxit = 2000, trControl = fitControl)

model_bayesglm1 = train(possessions ~ cummean_poss + cummean_poss.opp + slugTeam + slugTeam.opp
                  , data=trainData, method='bridge',trControl = fitControl)

model_lm1
model_lm2
model_lm3
model_lm4

model_rf1
model_rf2

model_nnet1
model_nnet2

model_lm1$results$Rsquared

preds_lm1 <- predict(model_lm1, testData)
preds_lm2 <- predict(model_lm2, testData)
preds_lm3 <- predict(model_lm3, testData)

postResample(preds_lm1, testData$possessions)
postResample(preds_lm2, testData$possessions)
postResample(preds_lm3, testData$possessions)

plot(testData$possessions, preds_lm3)

plot(x$possessions, predict(model_lm3, x))

plot(x$possessions, predict(model_lm2, x))
plot(x$possessions, predict(model_lm1, x))


plot(model_lm3)
ggplot(model_lm3)
 
summary(model_lm4)

x$preds <- predict(model_lm1,x)

plot(x$possessions, preds)

plot(model_lm1)

x %>% 
  mutate(delta = )
  group_by(slugTeam) %>%
  summarise()
  
summary(lm(pts ~ possessions + slugTeam, data = x))
  
plot(x$possessions, x$pts)

ggplot(model_df, aes(x=possessions, fill = restDays)) +
  geom_density(position = 'identity', alpha = .5)

hist(model_df$countDaysRestTeam, breaks = 150)


summary(lm(I(pts/possessions) ~ slugTeam, data = x))

ggplot(team_df, aes(x = dateGame, y = I(pts / possessions), group = slugTeam, color = slugTeam)) + 
  geom_line(alpha = 0.5)




ggplot(model_df, aes(x=((pts/possessions)*100) - cummean_ortg, fill=slugTeam)) + 
  geom_density(position='identity', alpha = 0.4)



group_by(model_df, slugTeam) %>%
  summarise(var = var( ((pts/possessions)*100) - cummean_ortg, na.rm = T )
            ) %>%
  ggplot(aes(x = var, y = reorder(slugTeam, -var), fill = slugTeam)) +
  geom_bar(stat = 'identity', width =1)

