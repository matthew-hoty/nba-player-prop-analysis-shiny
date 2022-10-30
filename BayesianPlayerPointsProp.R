library(nbastatR)
library(dplyr)
library(DescTools)

gl_df <- game_logs(seasons = 2022, 
          league = "NBA", 
          result_types = "player",
          season_types = "Regular Season")

bkn_games <- gl_df %>%
  filter(slugTeam == "BKN" & namePlayer == 'Kevin Durant') %>%
  select(idGame, slugTeam, namePlayer, locationGame, minutes, pts)

durant_game_pbp <- play_by_play_v2(game_ids = bkn_games$idGame)



# need to find which team is home and which is away
# need to parse the descriptions to understand which type of shot was made

#remove player name
#remove num'
#remove anything in ()

df <- durant_game_pbp %>%
  left_join(bkn_games) %>%
  filter(namePlayer1 == 'Kevin Durant')  %>%
  #filter(idGame == '22100001') %>%
  mutate(descriptionPlay = if_else(locationGame == 'H', descriptionPlayHome, descriptionPlayVisitor),
         desc = trimws(gsub("Durant|\\(.*\\)|\\d+'",'',df$descriptionPlay)),
         fga = if_else(numberEventMessageType %in% c(1,2),1,0),
         fgm = if_else(numberEventMessageType %in% c(1),1,0),
         fg3a = if_else(numberEventMessageType %in% c(1,2) & desc %like% '3PT ',1,0),
         #fg3a = case_when(numberEventMessageType %in% c(1,2) & desc %like% '3PT ' ~ 1, TRUE ~ 0),
         fg3m = if_else(numberEventMessageType %in% c(1,2) & desc %like% '3PT ', 1,0),
         #fg3m = case_when(numberEventMessageType %in% c(1,2) & desc %like% '3PT ' ~ 1, TRUE ~ 0),
         fta = if_else(numberEventMessageType %in% c(6),1,0),
         ftm = if_else(numberEventMessageType %in% c(6) & numberEventActionType %in% c(2),1,0),
         pts = (fgm * 2) + fg3m + ftm,
         #cum_pts = cumsum(pts)
         ) %>%
  group_by(idGame) %>%
  mutate(cum_pts = cumsum(pts)) %>%
  select(idGame, timeRemaining, namePlayer1, 
         numberEventMessageType, numberEventActionType, numberPeriod, 
         desc, descriptionPlay,
         fga, fgm, fg3a, fg3m, fta, ftm, pts, cum_pts,
         scoreAway, scoreHome, marginScore)


action_type_df <- df %>%
  filter(namePlayer1 == 'Kevin Durant') %>%
  select(numberEventMessageType, numberEventActionType,desc) %>%
  filter(numberEventMessageType %in% c(1,2)) %>%
  unique() %>%
  arrange(numberEventActionType, numberEventMessageType)



#event message type:
# 1 = made shot
# 2 - missed shot
# 3 - free throw (11 - miss, 12 - make)
# 4 - reboud
# 5 - TO
# 6 - foul (1 - personal, 2 - shooting)
# 7 - ?
# 8 - Subbed out

###################################################################
##
## Historical Average
## Basically - can we see what expected points remaining based on averages?
##
###################################################################

# https://arxiv.org/pdf/1906.05029.pdf
# https://towardsdatascience.com/create-your-own-nfl-touchdown-props-with-python-b3896f19a588
# https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/

hist(bkn_games$pts,prob = TRUE, ylim = c(0,.1))
lines(min(bkn_games$pts):max(bkn_games$pts), 
      dpois(min(bkn_games$pts):max(bkn_games$pts),mean(bkn_games$pts))
      )

x2 <- seq(min(bkn_games$pts), max(bkn_games$pts), length = 55)
fun <-dpois(x2,mean(bkn_games$pts)) 
lines(x2,fun,col = 2, lwd = 2)

mean(bkn_games$pts);var(bkn_games$pts)


vec <- rpois(50, 3)
hist(vec, prob=TRUE, ylim = c(0, .25)) # may need to tweak the y axis.
lines(0:max(vec), dpois(0:max(vec), mean(vec)), col = 'red')



# X-axis grid
x2 <- seq(min(x), max(x), length = 40)

# Normal curve
fun <- dnorm(x2, mean = mean(x), sd = sd(x))

# Histogram
hist(x, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)




hist(rpois(55, 29.87))
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

