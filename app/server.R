#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(gdata)
library(scatterplot3d)
library(plotly)

cube_table<-read.csv("input/sim_table_chem_jaccard_pheno.csv")
toolscore_table<-read.csv("input/toolscore_mapped_lincs.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Selectize box for query genes
  output$select_genes = renderUI({
    selectizeInput('query_genes', 'Select Query Genes', 
                   choices = unique(cube_table$cmpd1), multiple = T)
  })
  
  output$threshold_slider = renderUI({
    sliderInput('threshold', label = "Selectivity Threshold",
                min = round_any(min(toolscore_table$selectivity, na.rm = T), 0.1, floor),
                max = round_any(max(toolscore_table$selectivity, na.rm = T), 0.1, ceiling),
                step = 0.1, value = 0.3)
  })
  
  # Observer for inputs
  observeEvent(c(input$query_genes, input$threshold), {
    ######################################################################################T
    # create grouped table -----
    ######################################################################################T
    query_cmpds<-unique(cube_table$cmpd1)
    cube_table.g<-dlply(cube_table,.(cmpd1),c)
    
    ######################################################################################T
    # Example query #1: show all compounds in relation to 10101 -----
    ######################################################################################T
    if(!is.null(input$query_genes)) {
      c.data<-as.data.frame(cube_table.g[[match(input$query_genes,query_cmpds)]])
    ## add additional qc filters
    c.data<-as.data.frame(cube_table.g[[match(input$query_genes,query_cmpds)]])%>%
      filter(n_pairs>5)%>%
      filter(n_common>5)
    
    c.data$cmpd2 = factor(c.data$cmpd2)
    p <- plot_ly(c.data, x = ~chem_sim, y = ~jaccard_sim, z = ~pearson_corr, color = ~cmpd2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Chemical Similarity',
                                       range = c(-0.1,1.1)),
                          yaxis = list(title = 'Jaccard Similarity',
                                       range = c(-0.1,1.1)),
                          zaxis = list(title = 'Pearson Correlation',
                                       range = c(-1.1,1.1))))
    output$testplot <- renderPlotly(p)
    }
  })
})
