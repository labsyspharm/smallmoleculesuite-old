#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HMS Drug App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       uiOutput('select_genes'),
       uiOutput("threshold_slider"),
       sliderInput("n_common", "n_common value", min = 0, max = 15,
                   step = 1, value = 5),
       sliderInput("n_pairs", "n_pairs value", min = 0, max = 15,
                   step = 1, value = 5),
       verbatimTextOutput('best_inhibitor')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput('mainplot'),
       uiOutput('show_table')
       #dataTableOutput('data_table'),
       #dataTableOutput('toolscore_table')
    )
  )
))
