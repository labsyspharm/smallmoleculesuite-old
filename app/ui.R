#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HMS Drug App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       uiOutput('select_genes'),
       uiOutput("threshold_slider")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput('testplot')
    )
  )
))
