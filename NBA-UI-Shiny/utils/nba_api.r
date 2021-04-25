library(furrr)
library(httr)
library(dplyr)
library(magrittr)
library(future)
plan(multisession, workers=3 )

.season_maps <- list(
  '2021' = "2020-21",
  '2020' = "2019-20"
)
#season = 2021
boxscore_type <- 'Four Factors'
boxscore_type <- 'Base'
.call_api <- function(season, boxscore_type){
  print(boxscore_type)
  glue::glue("Finding Boxscores for {season} and {boxscore_type}")
  
  url <- 'https://stats.nba.com/stats/teamgamelogs'
  query_params <- list(
    'LeagueID' = '00',
    'MeasureType' = boxscore_type,
    'PerMode'='Totals',
    'Season' = .season_maps[as.character(season)],
    'SeasonType'= I('Regular+Season')
  )
  
  headers = c(
    "Accept" = '*/*',
    "Origin" =  'https://www.nba.com',
    "Accept-Encoding" = 'gzip, deflate, br',
    "Host" = 'stats.nba.com',
    "User-Agent" = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0.2 Safari/605.1.15',
    "Accept-Language" = 'en-us',
    "Referer" = 'https://www.nba.com/players',
    "Connection" = 'keep-alive',
    'x-nba-stats-token'= 'true',
    'x-nba-stats-origin'= 'stats'
  )
  
  request <- GET(url = url, 
                 query = query_params,
                 add_headers(.headers = headers))
  #request$url
  #content(request, as = "text", encoding = "UTF-8")
  response <- content(request, as = "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    pluck('resultSets') 
  
  df <- data.frame(response$rowSet[[1]])    
  colnames(df) <- response$headers[[1]]
  
  return(df)
}


get_team_boxscores <- 
  function(seasons = c(2021), 
           boxscore_types = c('Base','Advanced','Four Factors','Misc','Scoring') ) {
  
    input_df <-
      expand.grid(season = seasons,
                  boxscore = boxscore_types,
                  stringsAsFactors = F) %>%
      dplyr::as_tibble()
    
    call_api_safe <-
      purrr::possibly(.call_api, tibble())

    x <- lapply(1:nrow(input_df),
               function(x) {
                 df_row <-
                   input_df %>% slice(x)
                 
                 df_row %$%
                   .call_api(
                     season = season,
                     boxscore_type = boxscore
                   )}
               
               )
    
    all_data <-
      1:nrow(input_df) %>%
      future_map_dfr(function(x) {
        df_row <-
          input_df %>% slice(x)
        
        df_row %$%
          .call_api(
            season = season,
            boxscore_type = boxscore
          )
      })
    
    x <- all_data %>% distinct()
  
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
           idPlayer = as.double(PERSON_ID)
    ) %>%
    select(namePlayer,
           idPlayer,
           idTeam = TEAM_ID,
           slugTeam = TEAM_ABBREVIATION,
           position = POSITION,
           height = HEIGHT,
           weight = WEIGHT)
  
  return(new_df)
  
}