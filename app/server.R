
library(shiny)
#library(plyr)
library(dplyr)
#library(data.table)
#library(tidyr)

library(ggplot2)
library(plotly)

cube_table<-read.csv("input/sim_table_chem_jaccard_pheno.csv")
cube_table$cmpd2_name = cube_table$cmpd2
cube_table$cmpd2 = factor(cube_table$cmpd2)
toolscore_table<-read.csv("input/toolscore_mapped_lincs.csv")

shinyServer(function(input, output) {
  values = reactiveValues(cube_table = NULL, toolscore_table = NULL)
  # Selectize box for query genes
  output$select_genes = renderUI({
    fluidRow(
      radioButtons('query_type', "", choices = c("Query type 1", "Query type 2"), inline = T),
      conditionalPanel(condition = "input.query_type=='Query type 1'",
      selectizeInput('query_genes', 'Select Query Genes', 
                       choices = sort(unique(cube_table$cmpd1)), multiple = T)),
      conditionalPanel(condition = "input.query_type=='Query type 2'",
      selectizeInput('query_genes2', 'Select Query Genes (toolscore)', 
                     choices = sort(unique(toolscore_table$gene_id)), multiple = T))
    )
  })
  
  output$threshold_slider = renderUI({
    sliderInput('threshold', label = "Selectivity Threshold",
                min = plyr::round_any(min(toolscore_table$selectivity, na.rm = T), 0.1, floor),
                max = plyr::round_any(max(toolscore_table$selectivity, na.rm = T), 0.1, ceiling),
                step = 0.1, value = 0.3)
  })
  
  output$show_table = renderUI({
    fluidRow(
      radioButtons('which_data', "", choices = c("Cube table", "Toolscore table"), inline = T),
      conditionalPanel(condition = "input.which_data=='Cube table'",
                       dataTableOutput('data_table')),
      conditionalPanel(condition = "input.which_data=='Toolscore table'",
                       dataTableOutput('toolscore_table'))
    )
  })
                   
  observeEvent(c(input$query_genes, input$threshold, input$n_common, input$n_pairs), {
    values$cube_table = cube_table[cube_table$cmpd1 %in% input$query_genes & 
                                              cube_table$n_pairs > input$n_pairs &
                                              cube_table$n_common > input$n_common,]
    output$data_table = renderDataTable(values$cube_table)
  })
  
  observeEvent(c(input$query_genes2, input$threshold, input$n_common, input$n_pairs), {
    values$toolscore_table = toolscore_table[toolscore_table$gene_id %in% input$query_genes2 &
                                               toolscore_table$selectivity >= input$threshold &
                                               toolscore_table$source_id %in% unique(cube_table$cmpd1),] %>%
      arrange(desc(tool_score))
    print(head(toolscore_table))
    print(head(values$toolscore_table))
    c.best_inhibitor <- values$toolscore_table$source_id[1]
    c.best_inhibitor_name = values$toolscore_table$name[1]
    output$best_inhibitor = renderPrint(c.best_inhibitor_name)
    output$toolscore_table = renderDataTable(values$toolscore_table)

    values$cube_table = cube_table[cube_table$cmpd1 == c.best_inhibitor &
                                   cube_table$cmpd2_name %in% unique(values$toolscore_table$source_id) & 
                                   cube_table$n_pairs > input$n_pairs &
                                   cube_table$n_common > input$n_common,]
    output$data_table = renderDataTable(values$cube_table)
  })
  
  observeEvent(values$cube_table, {
      p1 <- plot_ly(values$cube_table, x = ~chem_sim, y = ~jaccard_sim, color = ~cmpd2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Chemical Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'Jaccard Similarity',
                                       range = c(-0.1, 1.1))))
    p2 <- plot_ly(values$cube_table, x = ~chem_sim, y = ~pearson_corr, color = ~cmpd2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Chemical Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'Pearson Correlation',
                                       range = c(-1.1,1.1))))
    p3 <-  plot_ly(values$cube_table, x = ~pearson_corr, y = ~jaccard_sim, color = ~cmpd2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Pearson Correlation',
                                       range = c(-1.1, 1.1)),
                          yaxis = list(title = 'Jaccard Similarity',
                                       range = c(-0.1, 1.1))))
    p <- subplot(p1, p2, p3, shareX = F, shareY = F, titleX = T, titleY = T)
    output$mainplot <- renderPlotly(p)
  })
})
