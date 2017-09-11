library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggvis)

affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")
affinity_selectivity$selectivity_class[is.na(affinity_selectivity$selectivity_class)] = "unknown"
selectivity_order<-c("best_class","second_class","non_specific","unknown","other")

# Function for toolip values
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
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
  
      # ggplot(values$c.binding_data, aes(x = selectivity, y = mean_affinity, color = selectivity_class)) +
      #   geom_point() +
      #   scale_y_log10(limits = c((10^input$affinity[1] + 0.01), 10^input$affinity[2])) +
      #   scale_x_continuous(limits = c(0, max(values$c.binding_data$selectivity,na.rm=T)))
      values$c.binding_data %>%
        ggvis(x = ~selectivity, y = ~mean_affinity, fill = ~selectivity_class, stroke = ~selectivity_class, fillOpacity := 0.5, strokeOpacity := 0.5) %>%
          scale_numeric("y", trans = "log", expand = 0) %>%
          #scale_numeric("x", range = c(-0.5, max(values$c.binding_data$selectivity, na.rm=T))) %>%
          layer_points(stroke.brush := "red") %>%
          #ggvis::hide_legend(c("fill", "stroke")) %>%
          set_options(height = 300, width = 500) %>%
          #add_axis("x", title = "x_name") %>%
          #add_axis("y", title = "y_name") %>%
          add_tooltip(all_values, on = c("hover", "click")) %>%
          bind_shiny("mainplot")
    }
  }, ignoreInit = T)

  # selected_1<-10411
  # selected_2<-10103
  # selected_3<-10175
  # 
  # selection1.binding_data = affinity_selectivity %>%
  #   filter(hms_id==selected_1)%>%
  #   filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  #   filter(SD_affinity<=max_sd)%>%
  #   filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  #   arrange(mean_affinity,selectivity_class)
  # selection1.title<-paste0(unique(selection1.binding_data$hms_id),";",unique(selection1.binding_data$name))
  # selection1.display_table<-selection1.binding_data[1:7,c(3,4,5)]

  
  # display results table
      output$output_table = DT::renderDataTable({
        datatable(values$selection_table, extensions = c('Buttons', 'FixedHeader'),
                  filter = 'top',
                  rownames = F, options = list(
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
            "}"),
          searchHighlight = TRUE,
          fixedHeader = TRUE,
          autoWidth = TRUE
        ))
                  }, server = FALSE)
})