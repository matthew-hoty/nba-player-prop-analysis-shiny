#library(stringr)
#library(DescTools)
#library(RCurl)
library(dplyr)
library(nbastatR)
library(aws.s3)
library(lubridate)
library(httr)
library(jsonlite)
library(purrr)

Sys.setenv("AWS_PROFILE" = "cli")
Sys.setenv("DATA_BUCKET" = 'scraper-prod-269350797537')

get_latest_fanduel_file <- function(){
  bucket = Sys.getenv('DATA_BUCKET')
  bucket_contents = get_bucket(bucket = bucket)
  bucket_df <- unstack(data.frame(d<-unlist(bucket_contents),sub(".*[.]","",names(d))))
  
  key <- bucket_df %>%
    filter(str_detect(Key,"^nba/")) %>%
    filter(str_detect(Key,'selection')) %>%
    filter(LastModified == max(LastModified)) %>%
    select(Key) %>%
    unlist()

  df <- s3read_using(FUN=read.csv, bucket = bucket, object = key ) 
  return(df)
}

get_schedule <- function(){
  schedule_all_df = current_schedule()
  
  home_sched_df <- schedule_all_df %>%
    select(dateGame, idGame, slugTeam = slugTeamHome, idTeam = idTeamHome) %>% mutate(locationGame = 'H')
  away_sched_df <- schedule_all_df %>%
    select(dateGame, idGame, slugTeam = slugTeamAway, idTeam = idTeamAway) %>% mutate(locationGame = 'A')
  combined_sched_df <- bind_rows(home_sched_df,away_sched_df)
  schedule_df <- combined_sched_df %>%
    left_join(select(combined_sched_df,idGame,slugTeam, idTeam), by = c("idGame" = "idGame"), suffix=c('','Opp')) %>%
    filter(slugTeam != slugTeamOpp) 
  
  return(schedule_df)
}

get_boxscores <- function(){
  today = format(Sys.Date(), format="%m-%d-%Y")
  filename <- paste0("boxscore_data/boxscores_",today,".RDS")
  
  if(file.exists(filename)){
    print('LOADING BOXSCORES FROM MEMORY')
    gl_df <- readRDS(filename)
  } else{
    gl_df <- game_logs(seasons = 2020:2021) %>%
      mutate(slugOppLoc = ifelse(locationGame == 'H', paste('v.',slugOpponent),paste('@',slugOpponent)))

    print('SAVING BOXSCORES TO MEMEORY')
    saveRDS(gl_df,filename)
  } 
  return(gl_df)
}

get_player_data <- function(){
  path <- 'https://stats.nba.com/stats/playerindex'
  query_params <- list(
    Height='',
    LeagueID='00',
    Season='2020-21',
    SeasonType='Regular%20Season',
    TeamID='0',
    Weight=''
  )
  
  headers = c(
    "Accept" = '*/*',
    "Origin" =  'https://www.nba.com',
    "Accept-Encoding" = 'gzip, deflate, br',
    "Host" = 'stats.nba.com',
    "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0.2 Safari/605.1.15',
    "Accept-Language" = 'en-us',
    "Referer" = 'https://www.nba.com/players',
    "Connection" = 'keep-alive'
  )
  
  request <- GET(url = path, 
                 query = query_params,
                 add_headers(.headers = headers
                 )
  )
  
  response <- content(request, as = "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    pluck('resultSets') 
  
  df <- data.frame(response$rowSet[[1]])    
  colnames(df) <- response$headers[[1]]
  
  df$HEIGHT <- sapply(strsplit(df$HEIGHT,"-"), function(x){
    (as.numeric(x[1])*12) + as.numeric(x[2])
  })
  
  new_df <- df %>% 
    mutate(namePlayer = paste(PLAYER_FIRST_NAME,PLAYER_LAST_NAME),
           idPlayer = as.double(PERSON_ID),
           idTeam = as.double(TEAM_ID)
           ) %>%
    select(namePlayer,
           idPlayer,
           idTeam,
           slugTeam = TEAM_ABBREVIATION,
           position = POSITION,
           height = HEIGHT,
           weight = WEIGHT)
  
  return(new_df)
  
}

get_todays_data <- function(){
  
  return(
    list(
      wagers_df = get_latest_fanduel_file(),
      game_logs_df = get_boxscores(), #game_logs(seasons = 2020:2021),
      schedule_df = get_schedule(),
      player_df = get_player_data()
    )
  )  
}