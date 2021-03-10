# NBA Player Prop Analysis Tool

![Shiny Example](shiny-example.gif)

This Shiny app provided a quick way to do analysis on FanDuel player props for tonights games.  Selecting a prop will dynamically generate a graph of recent player performance to help inform betting decisions.  

### Process
* Read CSV files generated from [NBA-FanDuel-Scraper](https://github.com/matthew-hoty/nba-fanduel-scraper)
* Clean data and filter to player props
* Join recent performance data
* Calculate my predicted probability for a given wager
* Compare with implied propbabilty from FanDuel odds and rank by percieved edge

### Setup 
* R version 4.0.3 

### Run Shiny
