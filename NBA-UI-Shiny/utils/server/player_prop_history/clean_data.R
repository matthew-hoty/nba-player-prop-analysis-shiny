library(dplyr)


#### Clean Wager Data ####
clean_wagerData <- function(df){
  new_df <- df %>%
    .clean_wagerData_handicap() %>%
    .clean_wagerData_hadValue() %>%
    .clean_wagerData_namePlayer() %>%
    .clean_wagerData_marketDesc()
  return(new_df)
  
}

.clean_wagerData_handicap <- function(df){
  new_df <- df %>% 
    mutate(
      currentHandicap = case_when(
        !is.na(currentHandicap) ~ currentHandicap,
        !is.na(str_extract(marketName,'[0-9]+')) ~ as.numeric(str_extract(marketName,'[0-9]+')),
        !is.na(str_extract(selectionName,'\\d+\\.*\\d*')) ~ as.numeric(str_extract(selectionName,'\\d+\\.*\\d*'))
      )
    )
  return(new_df)
}

.clean_wagerData_hadValue <- function(df){
  new_df <- df %>%
    mutate(
      hadvalue = case_when(
        hadvalue != '' & !is.na(hadvalue) ~ hadvalue,
        tolower(selectionName) %like% '%over%' ~ 'O',
        tolower(selectionName) %like% '%under%' ~ 'U',
        !is.na(str_extract(marketName,'\\+')) ~ 'O',
        TRUE ~ hadvalue
      )
    )
  return(new_df)
}

.clean_wagerData_namePlayer <- function(df){
  new_df <- df %>%
    mutate(
      namePlayer = trimws(gsub('Over|Under|[0-9\\.]+','',selectionName))
    )
  return(new_df)
}

.clean_wagerData_marketDesc <- function(df){
  new_df <- df %>%
    mutate(
      marketDesc = case_when(
        marketType %like any% c('To Score X Points%',
                                'To Record X Assists%',
                                'To Record X Rebounds%',
                                'Made X Threes%') ~ paste(namePlayer, marketName),
        marketType %like% '%O/U%' ~ paste(selectionName, 
                                          currentHandicap, 
                                          str_replace_all(marketType,
                                                          c('Player X | O\\/U \\(E-Venue\\)'),
                                                          '')
                                          ),
        marketType %like% '% Alt %' ~ paste(selectionName,
                                            str_replace_all(marketType,
                                                            c('Player X Alt | - Any \\(E-Venue\\)'),
                                                            '')
                                            ),
        TRUE ~ marketType
      )
    )
  return(new_df)
}


