library(shiny)
library(dplyr)
library(readr)
library(DT)
library(plotly)
library(crosstalk)

affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv") %>% mutate(`log10_mean_Kd_(nM)` = log10(`mean_Kd_(nM)`))
selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")

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
  
  # show/hide filters
  observeEvent(input$filter_button, {
    toggleElement(id = "filters", anim = T, animType = "fade")
    toggleElement(id = "filter_down")
    toggleElement(id = "filter_right")
  })
  
  observeEvent(input$include_genes,{
    if(input$include_genes) {
      genes = sort(unique(affinity_selectivity$symbol))
      # all genes
    } else {
      genes = affinity_selectivity %>%
        filter(tax_id == 9606)
      genes = sort(unique(genes$symbol))
    # just human genes
    }
    output$query_gene_output = renderUI(
      selectizeInput(inputId = "query_gene", label = "", choices = genes,
        options = list(
          placeholder = 'Search for a gene target',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
  })
  
  observeEvent(c(input$query_gene, input$affinity, input$sd, input$min_measurements) , {
    if(input$query_gene != "") {
      showElement("loader1")
      showElement("plot_col")
      showElement("table_row")
      showElement("loader_table")
      
      values$c.binding_data = affinity_selectivity %>% 
        filter(symbol == input$query_gene) %>%
        filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
        filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
        filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`)
      
      if(!input$include_genes) {
        values$c.binding_data = values$c.binding_data %>%
          filter(tax_id == 9606)
      }
      
      ## show selection table -- user selects up to three compounds
      values$selection_table = values$c.binding_data
      # values$selection_table = values$c.binding_data[ , c("name", "hms_id",
      #   "symbol", "selectivity_class", "`mean_Kd_(nM)`", "selectivity", 
      #   "ontarget_IC50_Q1","offtarget_IC50_Q1", "offtarget_IC50_N")]
      
      d = SharedData$new(values$c.binding_data, ~name)
      
      ## plot data
      ##! we should include a solution for NA points as well.
      
      # display results table
      output$output_table = DT::renderDataTable({
        m2 <- values$c.binding_data[d$selection(), , drop = F]
        dt <- values$c.binding_data
        if(NROW(m2) == 0) {
          dt
        } else {
          m2
        }
      }, 
      extensions = 'Buttons',
      rownames = F, 
      options = list(
        dom = 'lBfrtip',
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        searchHighlight = TRUE,
        autoWidth = TRUE), server = F
      )
      
      output$mainplot <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~selectivity, y = ~`log10_mean_Kd_(nM)`, mode = "markers", 
                  color = I('black'), name = ~name, text = ~paste("Drug name: ", 
                    name, "\nDrug HMS ID: ", hms_id, "\nGene symbol: ", symbol,"\nx: ", selectivity, "\ny: ", 
                    `log10_mean_Kd_(nM)`, sep = ""), hoverinfo = "text") %>%
          # layout(showlegend = F,
          #        shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
          #                           line=list(dash='dot', width=2, color = "red")),
          #                      list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
          #                           line=list(dash='dot', width=2, color = "red"))),
          #        xaxis = list(range = c(-0.15, 1.15),
          #                     title = "Structural similarity",
          #                     tickmode = "array",
          #                     tickvals = c(-0.1, seq(0,1,.25)),
          #                     ticktext = c("NA", as.character(seq(0,1,.25))) ),
          #        yaxis = list(range = c(-1.2, 1.2),
          #                     title = "PFP",
          #                     tickmode = "array",
          #                     tickvals = c(-1.1, seq(-1,1,.5)),
          #                     ticktext = c("NA", as.character(seq(-1,1,.5))) )
          # ) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2), hoverinfo = "text")
      })
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
        filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
        filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
        filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`) %>%
        mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`))
      
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
  
  output$selection1 = DT::renderDataTable(
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
  
  output$selection2 = DT::renderDataTable(
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
  
  output$selection3 = DT::renderDataTable(
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