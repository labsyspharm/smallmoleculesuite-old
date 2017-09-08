library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(crosstalk)
library(d3scatter)
library(ggvis)

# load data
similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")
selectivity_order = c("best_class","second_class","non_specific","unknown","other")

# Function for toolip values
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

# hide tabs
tab.js = "$('.menu .item')
  .tab()
;"

# open about modal
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
    
    lb = linked_brush(keys = 1:dim(values$c.data)[1], "red") 
    selected = reactive({
      values$c.data[lb$selected(),]
    })

    values$c.data %>% 
      ggvis(x = ~structural_similarity, y = ~PFP, fill := "black", stroke:= "black", fillOpacity := 0.5, strokeOpacity := 0.5) %>%
      layer_points(stroke.brush := "red") %>%
      ggvis::hide_legend(c("fill", "stroke")) %>%
      set_options(height = 300, width = 300) %>%
      #add_axis("x", title = "x_name") %>%
      #add_axis("y", title = "y_name") %>%
      lb$input() %>%
      add_tooltip(all_values, on = c("hover", "click")) %>%
      layer_points(data = selected, x = ~structural_similarity, y = ~PFP, stroke.update := "red",
                   fillOpacity.update := 0, strokeOpacity.update := 0.5) %>%
      bind_shiny("mainplot1")
    
    values$c.data %>% 
      ggvis(x = ~structural_similarity, y = ~TAS, fill := "black", stroke := "black", fillOpacity := 0.5, strokeOpacity := 0.5) %>%
      layer_points(stroke.brush := "red") %>%
      ggvis::hide_legend(c("fill", "stroke")) %>%
      set_options(height = 300, width = 300) %>%
      #add_axis("x", title = "x_name") %>%
      #add_axis("y", title = "y_name") %>%
      lb$input() %>%
      add_tooltip(all_values, on = c("hover", "click")) %>%
      layer_points(data = selected, x = ~structural_similarity, y = ~TAS, stroke.update := "red",
                   fillOpacity.update := 0, strokeOpacity.update := 0.5) %>%
      bind_shiny("mainplot2")
    
    values$c.data %>% 
      ggvis(x = ~TAS, y = ~PFP, fill := "black", stroke:= "black", fillOpacity := 0.5, strokeOpacity := 0.5) %>% 
      layer_points(stroke.brush := "red") %>%
      ggvis::hide_legend(c("fill", "stroke")) %>%
      set_options(height = 300, width = 300) %>%
      #add_axis("x", title = "x_name") %>%
      #add_axis("y", title = "y_name") %>%
      lb$input() %>%
      add_tooltip(all_values, on = c("hover", "click")) %>%
      layer_points(data = selected, x = ~TAS, y = ~PFP, stroke.update := "red",
                   fillOpacity.update := 0, strokeOpacity.update := 0.5) %>%
      bind_shiny("mainplot3")
    
    output$data_table = renderDataTable( {
      values$c.data$selected = lb$selected()
      if(sum(lb$selected()) == 0) {
        values$c.data
      } else {
        DT::formatStyle(DT::datatable(values$c.data), "selected", target = "row",
                        color = DT::styleEqual(c(0, 1), c('black', 'white')),
                        backgroundColor = DT::styleEqual(c(0, 1), c('white', 'black')))
      }
      },
        extensions = c('Buttons', 'FixedHeader'),
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
          autoWidth = TRUE)
      )
    
    # Main table output
    output$binding_data = renderDataTable(
      values$c.display_table,
      extensions = c('Buttons', 'FixedHeader'),
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
        autoWidth = TRUE)
      )
    }, ignoreInit = T, ignoreNULL = T)
  
  # Make other tables on row selection
  observeEvent(input$data_table_rows_selected, {
    row = input$data_table_rows_selected
    print(row)
    for(i in length(row)) {
      name_data = paste("selection.binding_data", i, sep = "")
      name_display = paste("selection.display_table", i, sep = "")
      name_title = paste("selection.title", i, sep = "")
      
      drug = values$c.data$name_2[ row[i] ]
      print(drug)
      
      values[[name_data]] = affinity_selectivity %>%
        filter(name == drug) %>%
        filter(mean_affinity >= 10^input$affinity[1]) %>%
        filter(mean_affinity <= 10^input$affinity[2]) %>%
        filter(SD_affinity <= 10^input$sd) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, mean_affinity) %>%
        mutate(mean_affinity = round(mean_affinity))

      print(values[[name_data]])
      values[[name_display]] = values[[name_data]][,c(3,4,5)]
      values[[name_title]] = paste0(unique(values[[name_data]]$hms_id),";",unique(values[[name_data]]$name))
      print(values[[name_title]])
      
      output_name = paste("selection", i, sep = "")
      print(output_name)
      print(name_display)
      # output[[output_name]] = renderDataTable(
      #   values[[name_display]],
      #   extensions = c('Buttons'),
      #   rownames = F, options = list(
      #     dom = 't',
      #     buttons = c('copy', 'csv', 'excel', 'colvis'),
      #     initComplete = JS(
      #       "function(settings, json) {",
      #       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
      #       "}"),
      #     autoWidth = TRUE)
      # )
    }
  })
  
  output$selection1 = renderDataTable(
    values$selection.display_table1,
    extensions = c('Buttons'),
    rownames = F, options = list(
      dom = 't',
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )
  
  output$selection3 = renderDataTable(
    values$selection.display_table2,
    extensions = c('Buttons'),
    rownames = F, options = list(
      dom = 't',
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )
  
  output$selection3 = renderDataTable(
    values$selection.display_table3,
    extensions = c('Buttons'),
    rownames = F, options = list(
      dom = 't',
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )
  
  output$sel1_drug = renderText({ values$selection.title1 })
  output$sel2_drug = renderText({ values$selection.title2 })
  output$sel3_drug = renderText({ values$selection.title3 })
  
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