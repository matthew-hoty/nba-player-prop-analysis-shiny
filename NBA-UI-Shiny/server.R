#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("utils/server/helpers.R")
source('utils/server/plots/plot.R')

obj <- pred_wager_edge_df()
edge_df <- obj$edge_df
player_prop_hist_df <- obj$player_prop_hist_df
#schedule_df <- obj$schedule_df
#player_data <- obj$player_data
player_schedule_df <- obj$player_schedule_df

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$playerPlot <- renderPlot({

        generate_plots(player_prop_hist_df, 
                       edge_df,
                       player_schedule_df,
                       input$edgeDF_rows_selected)
    })
    
    output$edgeDF <- DT::renderDataTable({
        edge_df %>%
            #filter(Odds >= input$oddsRange[1] & Odds <= input$oddsRange[2]) %>%
            ungroup() %>%
            select(`Market`,
                    `Odds`, 
                    `My Prob`,
                    `House Prob`,
                    `My Value`)
        },
        selection = 'single'
    )
    
    output$playerDetailsDF <- DT::renderDataTable({
        if(is.null(input$edgeDF_rows_selected)){
            input_row <- 1
        } else {
            input_row <- input$edgeDF_rows_selected
        }
        print(input$edgeDF_rows_selected)
        selected_row <- edge_df[input_row,]
        player_summary_df <- player_prop_hist_df %>%
            filter(namePlayer %in% selected_row$Player &
                       marketName %in% selected_row$Wager &
                       marketDesc %in% selected_row$Market
            ) %>%
            select(dateGame, slugMatchup, minutes, pts, treb,ast,fgm,fga,fta,ftm,fg3m,fg3a)
        player_summary_df
    }, options = list(searching = FALSE,paging = FALSE), selection = 'none')

})
