library(shiny)
library(dplyr)
library(readr)
library(DT)

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
  
  # Jump to results tab when "Submit" is clicked
  # observeEvent(input$submitButton, {
  #   showElement("tab2_top")
  #   removeClass(id = "tab1_top", class = "active")
  #   removeClass(id = "tab1_bottom", class = "active")
  #   addClass(id = "tab2_top", class = "active")
  #   addClass(id = "tab2_bottom", class = "active")
  # })
  
  # Disable suspend for outputs to fix issue with certain outputs not updating 
  # after "submit" button is pushed... probably an issue with "shiny.semantic"
  # output$search_genes = renderText("")
  # output$gene_total = renderText("")
  # outputOptions(output, "search_genes", suspendWhenHidden = FALSE)
  # outputOptions(output, "gene_total", suspendWhenHidden = FALSE)

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