#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

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
            plotOutput("playerPlot")
        )
    )
))
