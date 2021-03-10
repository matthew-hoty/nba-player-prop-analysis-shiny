#The sorting should be based on a probability + expected return!! 
# I think we can use odds to extract a 'probability' and then can compare with our model and balance that against expected return (for things like ML or crazy low odds)
library(stringr)
library(DescTools)
library(ggplot2)
library(cowplot)
library(magick)
library(ggimage)
library(gridExtra)
library(png)
library(RCurl)
library(dplyr)
library(rsvg)
library(grid)
library(nbastatR)
library(ggrepel)
library(aws.s3)
library(lubridate)
options(scipen=999)

#### UDF #########################################
get_latest_fanduel_file <- function(){
  Sys.setenv("AWS_PROFILE" = "cli")
  bucket = 'scraper-prod-269350797537'
  bucket_contents = get_bucket(bucket = bucket)
  bucket_df <- unstack(data.frame(d<-unlist(bucket_contents),sub(".*[.]","",names(d))))
  key <- bucket_df$Key[which.max(parse_date_time(bucket_df$LastModified,"Ymd HMS"))]
  df <- s3read_using(FUN=read.csv, bucket = bucket, object = key ) 
  return(df)
}

get_schedule <- function(){
  schedule_all_df = current_schedule()
  
  home_sched_df <- schedule_all_df %>%
    select(dateGame, idGame, slugTeam = slugTeamHome) %>% mutate(locationGame = 'H')
  away_sched_df <- schedule_all_df %>%
    select(dateGame, idGame, slugTeam = slugTeamAway) %>% mutate(locationGame = 'A')
  combined_sched_df <- bind_rows(home_sched_df,away_sched_df)
  schedule_df <- combined_sched_df %>%
    left_join(select(combined_sched_df,idGame,slugTeam), by = c("idGame" = "idGame"), suffix=c('','Opp')) %>%
    filter(slugTeam != slugTeamOpp) 
  
  return(schedule_df)
}

evaluate_success <- function(ou,handicap,actual){
  #vectorize??
  return(ifelse(ou=='O', 
                ifelse(actual > handicap, 1, 0),
                ifelse(actual < handicap, 1, 0)
  )
  )
}  

#### Get Data #########################################
#wagers_all_df <- read.csv("/Users/matthewhoty/repos/nba-fanduel-scraper/Daily_Downloads/wagers_2021_02_16.csv")
#boxscore_all_df <- read.csv("data/team_boxscores_2021-2.csv")
wagers_all_df <- get_latest_fanduel_file()
game_log_df = game_logs(seasons = 2020:2021)
schedule_df <- get_schedule()

wagers_df <- wagers_all_df 

#### Manipulate Data #########################################
#I think first I should seperate player vs team bets
#then I need to standardize the format of over / under / handicap

#Team vs player wagers
wagers_df <- wagers_df %>%
  mutate(wagerType = case_when(
    marketName %in% c('Moneyline','Spread Betting','Total Points Scored') ~ 'Team',
    tolower(marketType) %like any% c('%team%','%margin%','%line%','%match%',
                                     '%race to%','%wire to wire%','alternate total points%','alternate handicaps%',
                                     'will there be overtime?%',
                                     '%1st%','%2nd%','%3rd%','%4th%','%first%','%second%','%third%','%fourth%') ~ 'Team',
    tolower(marketType) %like any% c('%player%','%to score x%','%to record x%','%made x threes%','%to record a triple double%') ~ 'Player'
  ))

## Need to figure out a way to dynamically calculate success 
player_wager_df <- wagers_df %>%
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

#### Calculate Stuff #########################################
#https://www.aceodds.com/bet-calculator/odds-converter.html
calc_wager_prob <- function(odds) {
  if(odds == 1){
    odds = .999999
  }
  if(odds > 0){
    value = 100 / (odds + 100)
  }else{
    value = (odds * -1) / (odds*-1 + 100)
  }
  return(round(value,2))
}

calc_wager_payout <- function(odds){
  if(odds > 0){
    value = 100 * ((1 + (odds / 100)) - 1)
  }else{
    value = 100 * ((1 - (100 / odds)) - 1)
  } 
  return(round(value,0))
}

convert_probs_to_odds <- function(probs){
  if(probs == 1){
    probs = .999999
  }
  if(probs < .5){
    value = (100 / (probs*100 / 100)) - 100
    #return((100 - (100*probs)) / probs)
  }else{
    value = ((probs*100) / (1 - ((probs*100)/100))) * -1
  }
  return(round(value,0))
}

convert_odds_to_dec <- function(odds){
  if(odds > 0){
    value = 1 + (odds/100)
    #return((100 - (100*probs)) / probs)
  }else{
    value = 1 - (100 / odds)
  }
  return(round(value,2))
}

#Positive odds - 1 plus (the american odds divided by 100) e.g. american odds of 
#300 = 1 + (300/100) = 4.
#Negative odds - 1 minus (100 divided by the american odds) e.g. american odds of 
#-300 = 1 - (100/-300) = 1.333.

breakdown <- player_wager_df %>%
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
  arrange(desc(pred_upside))

## entry point for R Shiny UI I think.
#### Have a table with the break down df, then click into a row to see the details

#### Graph Stuff #########################################
player_summary_df <- player_wager_df %>%
  filter(namePlayer == 'Kyle Lowry') %>%
  filter(marketName == 'To Record 14+ Assists') %>%
  mutate(avg_target = mean(target))

summary_tbl <- player_summary_df %>%
  group_by(namePlayer) %>%
  summarise(Odds = ifelse(mean(value) > 0, paste("+",mean(value),sep=''),mean(value)),
            `Matchup` = 'TBD', #paste(strwrap(first(gameDesc), width = 200),  collapse="\n"), #strwrap(first(gameDesc), width = 10, simplify = FALSE),
            `Avg Pts L10` = mean(ast),
            `Max Pts L10` = max(ast)) %>%
  select(-namePlayer) %>%
  t()

title_str = paste(first(player_summary_df$namePlayer),
                  first(player_summary_df$marketName))

(p1 <- ggplot(player_summary_df, aes(x = dateGame, y = ast)) +
  geom_point() + 
  geom_line() + 
  geom_hline(aes(yintercept = currentHandicap)) + 
  geom_hline(aes(yintercept = avg_pts), color = 'red', linetype = "dashed") + 
  #geom_label(aes(label=paste(pts,slugOppLoc)),vjust="inward",hjust="inward") + 
  geom_label_repel(aes(label=paste(pts,slugOppLoc))) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  theme_light())

img <- cowplot::ggdraw() + 
  cowplot::draw_image(player_summary_df$urlPlayerHeadshot[1],clip = 'on') +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

tbl <- tableGrob(summary_tbl,  cols = NULL, theme = ttheme_minimal())
layout <- rbind(c(1,3,3,3),
                c(1,3,3,3),
                c(2,3,3,3),
                c(NA,3,3,3)
)
g<- grid.arrange(img,tbl,p1, 
             nrow = 1, 
             layout_matrix = layout,
             top = textGrob(title_str, gp = gpar(fontsize = 15, fontface = 'bold'),x=0, hjust=-.1)#,hjust=1.15)
)




#tg <- textGrob(first(player_summary_df$marketName), gp = gpar(fontsize = 20, fontface = 'bold'),hjust=0)
#sg <- textGrob(first(player_summary_df$namePlayer), gp = gpar(fontsize = 15), hjust=0)














