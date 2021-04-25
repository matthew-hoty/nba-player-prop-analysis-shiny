#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)


table_graph_tab <- fluidRow(
    column(6,
           div(style = 'overflow-x: scroll', DT::dataTableOutput('edgeDF'))
    ),
    
    column(6,
           sliderInput("oddsRange", 
                       label = "Odds:",
                       min = -5000, max = 5000, value = c(-5000, 5000),sep = "",),
           plotOutput("playerPlot")
    )
)

shinyUI( 
    navbarPage("Bad Gambling Advice", theme = shinytheme("flatly"),
               tabPanel("Table",
                        table_graph_tab
               ),
               tabPanel("Parlays",
                        h2("Parlays tab")
               )
    )
)
if(FALSE){
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Player Prop Analysis Tool"),
    
    fluidRow(
        column(
            width = 6,
            div(style = 'overflow-x: scroll', DT::dataTableOutput('edgeDF'))
        ),
        
        column(6,
               sliderInput("oddsRange", 
                           label = "Odds:",
                           min = -5000, max = 5000, value = c(-5000, 5000),sep = "",),
            plotOutput("playerPlot")
        )
    )
))
}
