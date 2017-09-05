library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(crosstalk)
library(d3scatter)


similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")

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
  values = reactiveValues(c.data = NULL, c.data_title = NULL, c.binding_data = NULL,
                          c.display_table = NULL)
  
  # update the table upon parameter/input changes
  observeEvent(c(input$query_compound, input$n_common, input$n_pheno,
                 input$affinity, input$max_sd, input$min_measurements), {
    showElement("loader1")
    showElement("loader2")
    showElement("loader3")
    showElement("loader_tab")

    ## subset current data
    values$c.data = similarity_table %>%
      filter(name_1 == input$query_compound) %>%
      filter(n_assays_common_active >= input$n_common | is.na(n_assays_common_active)) %>%
      filter(n_pheno_assays_active_common >= input$n_pheno | is.na(n_pheno_assays_active_common)) %>%
      mutate_at(vars(PFP), funs(ifelse(is.na(PFP), -1.1, PFP))) %>%
      mutate_at(vars(TAS), funs(ifelse(is.na(TAS), -0.1, TAS))) %>%
      mutate_at(vars(structural_similarity), funs(ifelse(is.na(structural_similarity), -0.1, structural_similarity)))
      
    # ^are these the right filters?
    # filter by name or hms ID?
    
    values$c.data_title = paste0(unique(values$c.data$hmsID_1),";",unique(values$c.data$name_1))
    ## display scatterplots
    ## show c.data plotted w/selection boxes
    
    ## show affinity data of reference compound+ selected compounds
    selectivity_order = c("best_class","second_class","non_specific","unknown","other")
    
    # filter by name or hms id?
    values$c.binding_data = affinity_selectivity %>% filter(name == input$query_compound) %>%
      filter(mean_affinity >= 10^input$affinity[1]) %>%
      filter(mean_affinity <= 10^input$affinity[2]) %>%
      filter(SD_affinity <= 10^input$sd) %>%
      filter(n_measurements >= input$min_measurements) %>%
      mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
      arrange(selectivity_class, mean_affinity)
    
    #c.title<-paste0(unique(c.binding_data$hms_id),";",unique(c.binding_data$name))
    # title should be same as above, right?
    # by default it will display 10 rows at a time
    values$c.display_table = values$c.binding_data[,c(3,4,5)]
    
    # selection1.binding_data<-affinity_selectivity%>%filter(hms_id==selected_1)%>%
    #   filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
    #   filter(SD_affinity<=max_sd)%>%
    #   filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
    #   arrange(selectivity_class,mean_affinity)
    # selection1.title<-paste0(unique(selection1.binding_data$hms_id),";",unique(selection1.binding_data$name))
    # selection1.display_table<-selection1.binding_data[1:7,c(3,4,5)]
    # 
    # selection2.binding_data<-affinity_selectivity%>%filter(hms_id==selected_2)%>%
    #   filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
    #   filter(SD_affinity<=max_sd)%>%
    #   filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
    #   arrange(selectivity_class,mean_affinity)
    # selection2.title<-paste0(unique(selection2.binding_data$hms_id),";",unique(selection2.binding_data$name))
    # selection2.display_table<-selection2.binding_data[1:7,c(3,4,5)]
    
    #############
    # values$cube_table = cube_table[cube_table$cmpd1_name %in% input$query_compound & 
    #   cube_table$n_pairs > input$n_pairs &
    #   cube_table$n_common > input$n_common,] %>%
    #   mutate(chem_sim = round(chem_sim, 3))
    ############
    output$data_table = renderDataTable(values$c.data, extensions = c('Buttons', 'FixedHeader'),
        rownames = F, options = list(
          #columnDefs = list(list(visible=FALSE, targets=c(0,1))),
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
  
  observeEvent(values$c.data, {
    c.data_shared = SharedData$new(values$c.data)
    p1 <- d3scatter(data = c.data_shared, x = ~structural_similarity, y = ~PFP, color = ~name_2)
    p2 <- d3scatter(data = c.data_shared, x = ~structural_similarity, y = ~TAS, color = ~name_2)
    p3 <-  d3scatter(data = c.data_shared, x = ~TAS, y = ~PFP, color = ~name_2)
    
    output$mainplot1 = renderD3scatter(p1)
    output$mainplot2 = renderD3scatter(p2)
    output$mainplot3 = renderD3scatter(p3)
    #outputOptions(output, 'mainplot', suspendWhenHidden=FALSE)
  }, ignoreInit = T)
  
  # Brush/hover events
  # output$hover <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  # })
  # output$brush <- renderPrint({
  #   d <- event_data("plotly_selected")
  #   if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d
  # })
  
  # Click/zoom events
  # output$click <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })
  # output$zoom <- renderPrint({
  #   d <- event_data("plotly_relayout")
  #   if (is.null(d)) "Relayout (i.e., zoom) events appear here" else d
  # })
  # 
  #c.data_shared <- SharedData$new(values$c.data, ~rowname)
  # highlight selected rows in the scatterplot
  # output$p1 <- renderPlotly({
  #   s <- input$data_table_rows_selected
  #   if (!length(s)) {
  #     p <- d %>%
  #       plot_ly(x = ~mpg, y = ~disp, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
  #       layout(showlegend = T) %>%
  #       highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
  #   } else if (length(s)) {
  #     pp <- m %>%
  #       plot_ly() %>%
  #       add_trace(x = ~mpg, y = ~disp, mode = "markers", color = I('black'), name = 'Unfiltered') %>%
  #       layout(showlegend = T)
  # 
  #     # selected data
  #     pp <- add_trace(pp, data = m[s, , drop = F], x = ~mpg, y = ~disp, mode = "markers",
  #                     color = I('red'), name = 'Filtered')
  #   }
  # 
  # })
  # 
  # # highlight selected rows in the table
  # output$data_table <- DT::renderDataTable({
  #   m2 <- m[d$selection(),]
  #   dt <- DT::datatable(m)
  #   if (NROW(m2) == 0) {
  #     dt
  #   } else {
  #     DT::formatStyle(dt, "rowname", target = "row",
  #                     color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
  #                     backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
  #   }
  # })
  # 
  # # download the filtered data
  # output$x3 = downloadHandler('mtcars-filtered.csv', content = function(file) {
  #   s <- input$x1_rows_selected
  #   if (length(s)) {
  #     write.csv(m[s, , drop = FALSE], file)
  #   } else if (!length(s)) {
  #     write.csv(m[d$selection(),], file)
  #   }
  # })
  
  
})