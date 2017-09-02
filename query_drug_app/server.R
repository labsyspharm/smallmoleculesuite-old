library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)

cube_table = read_csv("input/sim_table_chem_jaccard_pheno.csv")
merge_table = read_csv("input/toolscore_mapped_lincs.csv")
cube_table$cmpd1_name = merge_table$name[match(cube_table$cmpd1, merge_table$source_id)]
cube_table$cmpd2_name = merge_table$name[match(cube_table$cmpd2, merge_table$source_id)]

tab.js = "$('.menu .item')
  .tab()
;"

about.modal.js = "$('.ui.mini.modal')
  .modal('show')
;"

shinyServer(function(input, output, session) {
  runjs(tab.js)
  # Define reactive values
  values = reactiveValues(test = NULL)
  # Make app stop when you close the webpage
  session$onSessionEnded(stopApp)
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # reactive values
  values = reactiveValues(cube_table = NULL)
  
  # update the table upon parameter/input changes
  observeEvent(c(input$query_compound, input$n_common, input$n_pairs), {
    showElement("loader1")
    showElement("loader2")
    values$cube_table = cube_table[cube_table$cmpd1_name %in% input$query_compound & 
      cube_table$n_pairs > input$n_pairs &
      cube_table$n_common > input$n_common,] %>%
      mutate(chem_sim = round(chem_sim, 3))
    output$data_table = renderDataTable(values$cube_table, extensions = c('Buttons', 'FixedHeader'),
        rownames = F, options = list(
          columnDefs = list(list(visible=FALSE, targets=c(0,1))),
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          searchHighlight = TRUE,
          fixedHeader = TRUE,
          autoWidth = TRUE))
    outputOptions(output, 'data_table', suspendWhenHidden=FALSE)
  }, ignoreInit = T, ignoreNULL = T)
  
  observeEvent(values$cube_table, {
    p1 <- plot_ly(values$cube_table, x = ~chem_sim, y = ~jaccard_sim, color = ~cmpd2_name) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Chemical Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'Jaccard Similarity',
                                       range = c(-0.1, 1.1))))
    p2 <- plot_ly(values$cube_table, x = ~chem_sim, y = ~pearson_corr, color = ~cmpd2_name) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Chemical Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'Pearson Correlation',
                                       range = c(-1.1,1.1))))
    p3 <-  plot_ly(values$cube_table, x = ~pearson_corr, y = ~jaccard_sim, color = ~cmpd2_name) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Pearson Correlation',
                                       range = c(-1.1, 1.1)),
                          yaxis = list(title = 'Jaccard Similarity',
                                       range = c(-0.1, 1.1))))
    p <- subplot(p1, p2, p3, shareX = F, shareY = F, titleX = T, titleY = T)
    output$mainplot <- renderPlotly(p)
    outputOptions(output, 'mainplot', suspendWhenHidden=FALSE)
  }, ignoreInit = T)
})