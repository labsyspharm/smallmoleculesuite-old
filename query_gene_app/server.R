library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggvis)

affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")
affinity_selectivity$selectivity_class[is.na(affinity_selectivity$selectivity_class)] = "unknown"
selectivity_order<-c("best_class","second_class","non_specific","unknown","other")
# add "id" column for tooltip
affinity_selectivity$id <- 1:nrow(affinity_selectivity)

# Function for toolip values
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- as.data.frame(affinity_selectivity[affinity_selectivity$id == x$id, c("name", "hms_id", 
    "selectivity_class", "selectivity", "mean_affinity")])
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

about.modal.js = "$('.ui.mini.modal')
  .modal('show')
;"

shinyServer(function(input, output, session) {
  # Make app stop when you close the webpage
  session$onSessionEnded(stopApp)
  
  # reactive values
  values = reactiveValues(c.binding_data = NULL, selection_table = NULL)
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  search_api <- function(affinity_selectivity, q){
    has_matching <- function(field) {
      grepl(q, field, ignore.case = T)
    }
    affinity_selectivity %>%
      mutate(symbol = as.character(symbol)) %>%
      arrange(symbol) %>%
      select(symbol) %>%
      unique %>%
      filter(has_matching(symbol)) %>%
      head(5) %>%
      transmute(name = symbol,
                value = symbol)
  }
  
  search_api_url = shiny.semantic::register_search(session, affinity_selectivity, search_api)
  output$gene_search = shiny::renderUI(search_selection_api("query_gene", search_api_url, multiple = FALSE))

  
  observeEvent(c(input$query_gene, input$affinity, input$sd, input$min_measurements) , {
    if(input$query_gene != "") {
      showElement("loader1")
      showElement("plot_col")
      showElement("table_row")
      showElement("loader_table")
      values$c.binding_data = affinity_selectivity %>% 
      filter(symbol == input$query_gene) %>%
      filter(mean_affinity >= 10^input$affinity[1]) %>%
      filter(mean_affinity <= 10^input$affinity[2]) %>%
      filter(SD_affinity <= 10^input$sd) %>%
      filter(n_measurements >= input$min_measurements) %>%
      mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
      mutate(mean_affinity = round(mean_affinity, 3)) %>%
      arrange(selectivity_class, mean_affinity)
      
      ## show selection table -- user selects up to three compounds
      values$selection_table = values$c.binding_data[ , c("name", "hms_id", "symbol", "selectivity_class",
                                                          "mean_affinity", "selectivity", "ontarget_IC50_Q1",
                                                          "offtarget_IC50_Q1", "offtarget_IC50_N")]
    ## plot data
    ##! we should include a solution for NA points as well.
      lb = linked_brush(keys = values$c.binding_data$id, "red") 
      selected = reactive({
        values$selection_table[lb$selected(),]
      })
      
      # display results table
      output$output_table = DT::renderDataTable({
        print(which(lb$selected()))
        if(sum(lb$selected()) == 0) {
          dt = values$selection_table
        } else {
          dt = values$selection_table[lb$selected(),]
        }
        datatable(dt, extensions = c('Buttons'),
                  filter = 'top',
                  rownames = F, options = list(
                    dom = 'lBfrtip',
                    buttons = c('copy', 'csv', 'excel', 'colvis'),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
                      "}"),
                    searchHighlight = TRUE,
                    autoWidth = TRUE
                  ))
      }, server = FALSE)
      
      values$c.binding_data %>%
        ggvis(x = ~selectivity, y = ~mean_affinity, fill = ~selectivity_class, stroke = ~selectivity_class, fillOpacity := 0.5, strokeOpacity := 0.5, key := ~id) %>%
          layer_points(stroke.brush := "red") %>%
          scale_numeric("y", trans = "log", expand = 0) %>%
          #scale_numeric("x", range = c(-0.5, max(values$c.binding_data$selectivity, na.rm=T))) %>%
          layer_points(stroke.brush := "red") %>%
          #ggvis::hide_legend(c("fill", "stroke")) %>%
          set_options(height = 680, width = "auto", resizable = T) %>%
          lb$input() %>%
          #add_axis("x", title = "x_name") %>%
          #add_axis("y", title = "y_name") %>%
          add_tooltip(all_values, on = c("hover", "click")) %>%
          bind_shiny("mainplot")
    }
  }, ignoreInit = T)

  # Make other tables on row selection
  observeEvent(input$output_table_rows_selected, {
    showElement("result_row3")
    row = input$output_table_rows_selected
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
      drug = values$selection_table$name[ row[i] ]
      
      values[[name_data]] = affinity_selectivity %>%
        filter(name == drug) %>%
        filter(mean_affinity >= 10^input$affinity[1]) %>%
        filter(mean_affinity <= 10^input$affinity[2]) %>%
        filter(SD_affinity <= 10^input$sd) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, mean_affinity) %>%
        mutate(mean_affinity = round(mean_affinity))
      
      values[[name_display]] = values[[name_data]][1:7,c(3,4,5)]
      print(head(values[[name_display]]))
      if(length(values[[name_data]]$hms_id) == 0) {
        values[[name_title]] = drug
      } else {
        values[[name_title]] = paste0(unique(values[[name_data]]$hms_id),"; ", drug)
      }
      output_name = paste("selection", i, sep = "")
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  proxy = dataTableProxy('output_table')
  
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
})