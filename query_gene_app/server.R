library(shiny)
library(dplyr)
library(readr)
library(DT)
library(plotly)
library(crosstalk)

affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv") %>% mutate(selectivity_plot = coalesce(selectivity, -0.5))

selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")

zipped_csv <- function(df_list, zippedfile, filenames, stamp) {
  dir = tempdir()
  mkdir = paste0("mkdir ", dir, "/", stamp)
  system(mkdir)
  len = length(df_list)
  for(i in 1:len) {
    # filename in temp directory 
    assign(paste0("temp",i), paste0(dir, "/", stamp, "/", filenames[i], ".csv"))
    # write temp csv
    write_csv(df_list[[i]], path=get(paste0("temp",i)))
  }
  # zip temp csv
  print(dir)
  print(filenames)
  zip(zippedfile, paste0(dir,"/", stamp, "/", filenames, ".csv"), flags = "-j" )
  # delete temp csv
  for(i in 1:len) {
    unlink( paste0("temp",i) )
  }
}

about.modal.js = "$('.ui.mini.modal')
  .modal('show')
;"

shinyServer(function(input, output, session) {
  # Make app stop when you close the webpage
  session$onSessionEnded(stopApp)
  
  # reactive values
  values = reactiveValues(c.binding_data = NULL, selection_table = NULL,
                          num_selected = 0)
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
  
  observeEvent(input$query_gene, {
    output$plot_title = renderText(paste0("Affinity and selectivity for drugs targeting ", input$query_gene))
    output$table_title = renderText(paste0("Data for drugs targeting ", input$query_gene))
  })
  
  observeEvent(c(input$query_gene, input$affinity, input$sd, input$min_measurements) , {
    if(input$query_gene != "") {
      showElement("loader1")
      showElement("plot_col")
      showElement("table_row")
      showElement("loader_table")
      showElement("plot_column")
      
      values$c.binding_data = affinity_selectivity %>%
        filter(symbol == input$query_gene) %>%
        filter(`mean_Kd_(nM)` >= 10^input$affinity[1] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`mean_Kd_(nM)` <= 10^input$affinity[2] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`SD_Kd_(nM)` <= 10^input$sd | is.na(`SD_Kd_(nM)`)) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`)
      
      if(!input$include_genes) {
        values$c.binding_data = values$c.binding_data %>%
          filter(tax_id == 9606)
      }
      
      values$selection_table = values$c.binding_data
      d = SharedData$new(values$c.binding_data, ~name)
      
      # display results table
      output$output_table = DT::renderDataTable({
        m2 = values$c.binding_data[d$selection(), 
          -which(names(values$c.binding_data) %in% c("selectivity_plot")), drop = F]
        dt <- values$c.binding_data[ , -which(names(values$c.binding_data) %in% 
                                                c("selectivity_plot")), drop = F]
        if(NROW(m2) == 0) {
          dt
        } else {
          m2
        }
      }, 
      extensions = 'Buttons',
      rownames = F, 
      options = list(
        columnDefs = list(list(visible=F, targets=match( c("investigation_bias", 
          "wilcox_pval", "IC50_diff"), names(values$c.binding_data)) - 1 )),
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
          plot_ly(x = ~selectivity_plot, y = ~`mean_Kd_(nM)`, mode = "markers", 
                  color = ~selectivity_class, text = ~paste("Drug name: ", 
                    name, "\nDrug HMS ID: ", hms_id, "\nGene symbol: ", symbol,"\nx: ", selectivity, "\ny: ", 
                    `mean_Kd_(nM)`, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = T,
                 shapes = list(list(type='line', x0= -0.5, x1= -0.5, y0= 10^(input$affinity[1]), y1= 10^(input$affinity[2]),
                    line=list(dash='dot', width=2, color = "red"))),
                 xaxis = list(range = c(-0.6, 1.3),
                              title = "Selectivity",
                              tickmode = "array",
                              tickvals = c(-0.5, seq(-0.25, 1.25, .25)),
                              ticktext = c("NA", as.character(seq(-0.25, 1.25, .25)))),
                 yaxis = list(range = c(input$affinity[1], input$affinity[2]),
                              title = "Mean Kd (nM)",
                              type = "log")
                 ) %>%
          highlight("plotly_selected", color = I('red'), hoverinfo = "text")
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
      name_file = paste0("selection.drug", i)
      drug = values$selection_table$name[ row[i] ]
      hms_id = values$selection_table$hms_id[ row[i] ]
      values[[name_title]] = paste0(hms_id, "; ", drug)
      values$num_selected = length(row)
      values[[name_file]] = drug
      
      values[[name_data]] = affinity_selectivity %>%
        filter(name == drug) %>%
        filter(`mean_Kd_(nM)` >= 10^input$affinity[1] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`mean_Kd_(nM)` <= 10^input$affinity[2] | is.na(`mean_Kd_(nM)`)) %>%
        filter(`SD_Kd_(nM)` <= 10^input$sd | is.na(`SD_Kd_(nM)`)) %>%
        filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`) %>%
        mutate(`mean_Kd_(nM)` = round(`mean_Kd_(nM)`, 3))
      
      values[[name_display]] = values[[name_data]][,c(3,4,5)]
      output_name = paste("selection", i, sep = "")
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  proxy = dataTableProxy('output_table')
  
  observeEvent(input$clearButton, {
    proxy %>% selectRows(NULL)
    for(i in 1:3) {
      assign(paste0("values$selection.binding_data",i), NULL)
    }
    values$num_selected = 0
  })
  
  output$selection1 = DT::renderDataTable(
    values$selection.display_table1,
    extensions = c('Buttons'),
    rownames = F, options = list(
      dom = 'tp',
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
      dom = 'tp',
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
      dom = 'tp',
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
  
  output$downloadBind <- downloadHandler(
    filename = function() {
      return(paste0("BindingData_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                    ".zip", sep = ""))
    },
    content = function(filename) {
      files_all = list(values$selection.binding_data1,
                       values$selection.binding_data2,
                       values$selection.binding_data3)
      # take only tables that exist
      drugs = NULL
      if(values$num_selected > 0) {
        files = files_all[1:values$num_selected]
        for(i in 1:3) {
          drugs = c(drugs, values[[paste0("selection.drug", i)]])
        }
      } else {
        files = NULL
        drugs = NULL
      }
      zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
    }, contentType = "application/zip"
  )
})