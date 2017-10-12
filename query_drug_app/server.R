library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(crosstalk)

# load data
similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")
selectivity_order = c("best_class","second_class","non_specific","unknown","other")

# Function for toolip values
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

# open about modal
about.modal.js = "$('.ui.mini.modal')
  .modal('show')
;"

shinyServer(function(input, output, session) {
  # Define reactive values
  values = reactiveValues(test = NULL)
  # Make app stop when you close the webpage
  #session$onSessionEnded(stopApp)

  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  observeEvent(input$query_compound, {
    output$binding_drug = renderText(paste0("Gene target binding data for ",input$query_compound, ":"))
  })

  # search_api <- function(similarity_table, q){
  #   has_matching <- function(field) {
  #     grepl(q, field, ignore.case = T)
  #   }
  #   similarity_table %>%
  #     mutate(name_1 = as.character(name_1)) %>%
  #     arrange(name_1) %>%
  #     select(name_1) %>%
  #     unique %>%
  #     filter(has_matching(name_1)) %>%
  #     head(5) %>%
  #     transmute(name = name_1,
  #               value = name_1)
  # }
  # 
  # search_api_url = shiny.semantic::register_search(session, similarity_table, search_api)
  # output$drug_search = shiny::renderUI(search_selection_api("query_compound", search_api_url, multiple = FALSE))

  observeEvent(input$query_compound, {
    if(length(input$query_compound) > 0) {
      values$drug_select = input$query_compound
    }
  })

  # reactive values
  values = reactiveValues(c.data = NULL, c.data_title = NULL, c.binding_data = NULL,
                          c.display_table = NULL, drug_select = NULL)
  
  # show/hide intro
  observeEvent(input$intro_hide, {
    toggleElement(id = "intro", anim = T, animType = "fade")
    toggleElement(id = "caret_down")
    toggleElement(id = "caret_right")
  })
  
  # show/hide filters
  observeEvent(input$filter_button, {
    toggleElement(id = "filters", anim = T, animType = "fade")
    toggleElement(id = "caret_down_fil")
    toggleElement(id = "caret_right_fil")
  })

  # update the table upon parameter/input changes
  observeEvent(c(values$drug_select, input$n_common, input$n_pheno), {
    if(!is.null(values$drug_select) & values$drug_select != "") {
    #hideElement(id = "intro", anim = T, animType = "fade", time = 1)
    showElement("filters_head")
    #showElement("filters")
    showElement("result_row1")
    showElement("result_row2")
    showElement("result_row4")
    showElement("loader1")
    showElement("loader2")
    showElement("loader3")
    showElement("loader_tab")

    ## subset current data
    values$c.data = similarity_table %>%
      filter(name_1 == values$drug_select) %>%
      filter(n_biol_assays_common_active >= 2^input$n_common | is.na(n_biol_assays_common_active)) %>%
      filter(n_pheno_assays_active_common >= 2^input$n_pheno | is.na(n_pheno_assays_active_common)) %>%
      mutate(PFP = round(PFP, 3), TAS = round(TAS, 3), structural_similarity = round(structural_similarity, 3)) %>%
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
    values$c.binding_data = affinity_selectivity %>% filter(name == values$drug_select) %>%
      #filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
      #filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
      #filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
      #filter(n_measurements >= input$min_measurements) %>%
      mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
      mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3)) %>%
      arrange(selectivity_class, `mean_Kd_(nM)`)

    d <- SharedData$new(values$c.data, ~name_2)

    #c.title<-paste0(unique(c.binding_data$hms_id),";",unique(c.binding_data$name))
    # title should be same as above, right?
    # by default it will display 10 rows at a time
    values$c.display_table = values$c.binding_data[,c(3,4,5)]

    output$mainplot1 <- renderPlotly({
      #s <- input$data_table_rows_selected
      #if (!length(s)) {
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~PFP, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, "\ny: ", 
            PFP, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = F,
                 xaxis = list(range = c(-0.15, 1.15),
                              title = "Structural similarity"),
                 yaxis = list(range = c(-1.2, 1.2),
                              title = "PFP")) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2), hoverinfo = "text")
      # } else if (length(s)) {
      #   pp <- values$c.data %>%
      #     plot_ly() %>% 
      #     add_trace(x = ~structural_similarity, y = ~PFP, mode = "markers", 
      #       color = I('black'), name = ~name_2, hoverinfo = "text") %>%
      #     layout(showlegend = F)
      #   
      #   # selected data
      #   pp <- add_trace(pp, data = values$c.data[s, , drop = F], 
      #     x = ~structural_similarity, y = ~PFP, mode = "markers",
      #     color = I('red'), name = ~name_2)
      # }
    })
    
    output$mainplot2 <- renderPlotly({
      #s <- input$data_table_rows_selected
      #if (!length(s)) {
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~TAS, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, 
            "\ny: ", TAS, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = F,
                  xaxis = list(range = c(-0.15, 1.15),
                               title = "Structural similarity"),
                  yaxis = list(range = c(-0.15, 1.15),
                               title = "TAS")) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
      # } else if (length(s)) {
      #   pp <- values$c.data %>%
      #     plot_ly() %>% 
      #     add_trace(x = ~structural_similarity, y = ~TAS, mode = "markers",
      #       color = I('black'), name = ~name_2) %>%
      #     layout(showlegend = F)
      #   
      #   # selected data
      #   pp <- add_trace(pp, data = values$c.data[s, , drop = F], 
      #     x = ~structural_similarity, y = ~TAS, mode = "markers",
      #       color = I('red'), name = ~name_2)
      # }
    })
    
    output$mainplot3 <- renderPlotly({
      #s <- input$data_table_rows_selected
      #if (!length(s)) {
        p <- d %>%
          plot_ly(x = ~TAS, y = ~PFP, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", TAS, "\ny: ", PFP, sep = ""),
            hoverinfo = "text") %>%
          layout(showlegend = F,
                 xaxis = list(range = c(-0.15, 1.15),
                              title = "TAS"),
                 yaxis = list(range = c(-1.2, 1.2),
                              title = "PFP")) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
      # } else if (length(s)) {
      #   pp <- values$c.data %>%
      #     plot_ly() %>% 
      #     add_trace(x = ~TAS, y = ~PFP, mode = "markers", color = I('black'), name = ~name_2) %>%
      #     layout(showlegend = F)
      #   
      #   # selected data
      #   pp <- add_trace(pp, data = values$c.data[s, , drop = F], 
      #     x = ~TAS, y = ~PFP, mode = "markers",
      #     color = I('red'), name = ~name_2)
      # }
    })

    output$data_table = renderDataTable( {
      m2 <- values$c.data[d$selection(), , drop = F]
      dt <- DT::datatable(values$c.data)
      if(NROW(m2) == 0) {
        dt
      } else {
        m2
      }
      },
        extensions = c('Buttons'),
        rownames = F, options = list(
          #columnDefs = list(list(visible=FALSE, targets=c(0,1))),
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          searchHighlight = TRUE,
          autoWidth = TRUE)
      )

    # Main table output
    output$binding_data = renderDataTable(
      values$c.display_table,
      extensions = c('Buttons'),
      rownames = F, options = list(
        dom = 'lBfrtip',
        #buttons = c('copy', 'csv', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        searchHighlight = TRUE,
        autoWidth = TRUE)
      )
    }
    }, ignoreInit = T, ignoreNULL = T)

  # Make other tables on row selection
  observeEvent(input$data_table_rows_selected, {
    showElement("result_row3")
    showElement("row3_bind_data")
    row = input$data_table_rows_selected
    # show/hide the selection tables
    if(length(row) == 1) {
      showElement("row3_col1")
      hideElement("row3_col2")
      hideElement("row3_col3")
      showElement("button_row")
    } else if(length(row) == 2) {
      showElement("row3_col1")
      showElement("row3_col2")
      hideElement("row3_col3")
      showElement("button_row")
    } else if(length(row) == 3) {
      showElement("row3_col1")
      showElement("row3_col2")
      showElement("row3_col3")
      showElement("button_row")
    }
    for(i in length(row)) {
      if(length(row) == 0) {
        hideElement("row3_col1")
        hideElement("row3_col2")
        hideElement("row3_col3")
        hideElement("button_row")
        break
      }
      if(length(row) > 3) { break }
      name_data = paste("selection.binding_data", i, sep = "")
      name_display = paste("selection.display_table", i, sep = "")
      name_title = paste("selection.title", i, sep = "")
      drug = values$c.data$name_2[ row[i] ]

      values[[name_data]] = affinity_selectivity %>%
        filter(name == drug) %>%
        #filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
        #filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
        #filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
        #filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`) %>%
        mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`))

      values[[name_display]] = values[[name_data]][,c(3,4,5)]
      if(length(values[[name_data]]$hms_id) == 0) {
        values[[name_title]] = drug
      } else {
        values[[name_title]] = paste0(unique(values[[name_data]]$hms_id),"; ", drug)
      }
      output_name = paste("selection", i, sep = "")
    }
  }, ignoreInit = T, ignoreNULL = F)

  proxy = dataTableProxy('data_table')

  observeEvent(input$clearButton, {
    proxy %>% selectRows(NULL)
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

  output$selection2 = renderDataTable(
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
