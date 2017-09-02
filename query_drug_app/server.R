library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)

cube_table = read.csv("../app/input/sim_table_chem_jaccard_pheno.csv")
cube_table$cmpd2_name = cube_table$cmpd2
cube_table$cmpd2 = factor(cube_table$cmpd2)

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
    values$cube_table = cube_table[cube_table$cmpd1 %in% input$query_compound & 
      cube_table$n_pairs > input$n_pairs &
      cube_table$n_common > input$n_common,]
    output$data_table = renderDataTable(values$cube_table, extensions = c('Buttons', 'FixedHeader'),
      filter = 'top', rownames = F, options = list(
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
    outputOptions(output, 'mainplot', suspendWhenHidden=FALSE)
  }, ignoreInit = T)
  
  # Jump to results tab when "Submit" is clicked
  # observeEvent(input$submitButton, {
  #   showElement("tab2_top")
  #   removeClass(id = "tab1_top", class = "active")
  #   removeClass(id = "tab1_bottom", class = "active")
  #   addClass(id = "tab2_top", class = "active")
  #   addClass(id = "tab2_bottom", class = "active")
  # })

  # Toggle filters when button is clicked
  # observeEvent(input$filter_button, {
  #   toggleElement(id = "filters", anim = T, animType = "fade")
  #   toggleElement(id = "filter_down")
  #   toggleElement(id = "filter_right")
  # })
  
  # display results table
  # observeEvent(input$table, {
  #     output$output_table = DT::renderDataTable({
  #       datatable(values$display_per_entry, extensions = c('Buttons', 'FixedHeader'),
  #                 filter = 'top',
  #                 rownames = F, options = list(
  #         dom = 'lBfrtip',
  #         buttons = c('copy', 'csv', 'excel', 'colvis'),
  #         initComplete = JS(
  #           "function(settings, json) {",
  #           "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
  #           "}"),
  #         searchHighlight = TRUE,
  #         fixedHeader = TRUE,
  #         autoWidth = TRUE
  #       ))
  #                 }, server = FALSE)
  # })
})