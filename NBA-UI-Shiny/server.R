#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("utils/helpers.R")
source('utils/plot.R')

obj <- pred_wager_edge_df()
edge_df <- obj$edge_df
player_prop_hist_df <- obj$player_prop_hist_df
#schedule <- obj$schedule_df

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$playerPlot <- renderPlot({

        generate_plots(player_prop_hist_df, edge_df,input$edgeDF_rows_selected)
    })
    
    output$edgeDF <- DT::renderDataTable(
        edge_df,
        selection = 'single'
    )

})
