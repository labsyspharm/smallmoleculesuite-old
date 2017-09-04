library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)


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

    ## subset current data
    values$c.data = similarity_table %>%
      filter(name_1 == input$query_compound) %>%
      filter(n_assays_common_active >= input$n_common | is.na(n_assays_common_active)) %>%
      filter(n_pheno_assays_active_common >= input$n_pheno | is.na(n_pheno_assays_active_common))
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
    p1 <- plot_ly(values$c.data, x = ~structural_similarity, y = ~PFP, color = ~name_2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Structural Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'PFP',
                                       range = c(-1.1, 1.1))))
    p2 <- plot_ly(values$c.data, x = ~structural_similarity, y = ~TAS, color = ~name_2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Structural Similarity',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'TAS',
                                       range = c(-0.1,1.1))))
    p3 <-  plot_ly(values$c.data, x = ~TAS, y = ~PFP, color = ~name_2) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'TAS',
                                       range = c(-0.1, 1.1)),
                          yaxis = list(title = 'PFP',
                                       range = c(-0.1, 1.1))))
    p <- subplot(p1, p2, p3, shareX = F, shareY = F, titleX = T, titleY = T)
    output$mainplot <- renderPlotly(p)
    outputOptions(output, 'mainplot', suspendWhenHidden=FALSE)
  }, ignoreInit = T)
})