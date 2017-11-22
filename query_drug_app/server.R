library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(crosstalk)

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

# load data
similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")
selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")

similarity_table$PFP_text = as.character(round(similarity_table$PFP, 3))
similarity_table$TAS_text = as.character(round(similarity_table$TAS, 3))
similarity_table$SS_text = as.character(round(similarity_table$structural_similarity, 3))


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
  # Make app stop when you close the webpage
  #session$onSessionEnded(stopApp)

  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  observeEvent(values$c.binding_display, {
    print(values$c.binding_display)
    
    # Binding table output
    output$binding_data = renderDataTable(
      if(dim(values$c.binding_display) > 0) {
        as.data.frame(values$c.binding_display)
      } else {
        NULL
      }, rownames = F, options = list(
        dom = 'tp',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        searchHighlight = TRUE,
        autoWidth = TRUE)
    )
    
    output$binding_drug = renderText(
      if(dim(values$c.binding_display)[1] > 0) {
        paste0(values$ref_hms_id, "; ", input$query_compound)
      } else {
        paste0("No gene target binding data available for ",input$query_compound)
      })
  })
  
  observeEvent(input$query_compound, {
    if(length(input$query_compound) > 0) {
      values$drug_select = input$query_compound
      output$ref_drug = renderText(
        paste0("Compound similarities for ", input$query_compound, 
               " from HMS LINCS small molecule library")
      )
    }
  })

  # reactive values
  values = reactiveValues(c.data = NULL, c.data_display = NULL, c.data_title = NULL, 
                          c.binding_data = NULL, c.binding_display = NULL, 
                          drug_select = NULL, num_selected = 0,
                          c.data_display_sub = NULL, c.data_sub = NULL)
  
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
    showElement("button_row")
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

    # Get rid of extra columns for display table
    values$c.data_display = values$c.data
    values$c.data_display$SS_text = NULL
    values$c.data_display$PFP_text = NULL
    values$c.data_display$TAS_text = NULL

    values$c.data_title = paste0(unique(values$c.data$hmsID_1),";",unique(values$c.data$name_1))

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

    values$ref_hms_id = unique(values$c.binding_data$hms_id)

    values$c.binding_display = values$c.binding_data[,c(3,4,5)]

    output$mainplot1 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~PFP, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", SS_text, "\ny: ", 
            PFP_text, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = F,
                 shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                             line=list(dash='dot', width=2, color = "red")),
                             list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                                  line=list(dash='dot', width=2, color = "red"))),
                 xaxis = list(range = c(-0.15, 1.15),
                              title = "Structural similarity",
                              tickmode = "array",
                              tickvals = c(-0.1, seq(0,1,.25)),
                              ticktext = c("NA", as.character(seq(0,1,.25))) ),
                 yaxis = list(range = c(-1.2, 1.2),
                              title = "PFP",
                              tickmode = "array",
                              tickvals = c(-1.1, seq(-1,1,.5)),
                              ticktext = c("NA", as.character(seq(-1,1,.5))) )
                              ) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2), hoverinfo = "text")
    })
    
    output$mainplot2 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~TAS, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", SS_text, 
            "\ny: ", TAS_text, sep = ""), hoverinfo = "text") %>%
          layout(showlegend = F,
                 shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0= -0.15, y1= 1.15,
                                    line=list(dash='dot', width=2, color = "red")),
                               list(type='line', x0= -0.15, x1= 1.15, y0= -0.1, y1= -0.1,
                                    line=list(dash='dot', width=2, color = "red"))),
                  xaxis = list(range = c(-0.15, 1.15),
                               title = "Structural similarity",
                               tickmode = "array",
                               tickvals = c(-0.15, seq(0,1,.25)),
                               ticktext = c("NA", as.character(seq(0,1,.25))) ),
                  yaxis = list(range = c(-0.15, 1.15),
                               title = "TAS",
                               tickmode = "array",
                               tickvals = c(-0.15, seq(0,1,.2)),
                               ticktext = c("NA", as.character(seq(0,1,.2))) )) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
    })
    
    output$mainplot3 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~TAS, y = ~PFP, mode = "markers", 
            color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
            name_1, "\nDrug 2: ", name_2, "\nx: ", TAS_text, "\ny: ", PFP_text, sep = ""),
            hoverinfo = "text") %>%
          layout(showlegend = F,
                 shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                                    line=list(dash='dot', width=2, color = "red")),
                               list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                                    line=list(dash='dot', width=2, color = "red"))),
                 xaxis = list(range = c(-0.15, 1.15),
                              title = "TAS",
                              tickmode = "array",
                              tickvals = c(-0.15, seq(0,1,.25)),
                              ticktext = c("NA", as.character(seq(0,1,.25))) ),
                 yaxis = list(range = c(-1.2, 1.2),
                              title = "PFP",
                              tickmode = "array",
                              tickvals = c(-1.2, seq(-1,1,.5)),
                              ticktext = c("NA", as.character(seq(-1,1,.5))))) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
    })

    output$data_table = renderDataTable( {
      values$c.data_display_sub <- values$c.data_display[d$selection(), , drop = F]
      values$c.data_sub = values$c.data[d$selection(), , drop = F]
      m2 = values$c.data_display_sub
      dt <- values$c.data_display
      if(NROW(m2) == 0) {
        dt
      } else {
        m2
      }
      },
      extensions = 'Buttons',
        rownames = F, options = list(
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
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
      hideElement("row3_col4")
      hideElement("row3_col5")
      #showElement("button_row")
    } else if(length(row) == 2) {
      showElement("row3_col1")
      showElement("row3_col2")
      hideElement("row3_col3")
      hideElement("row3_col4")
      hideElement("row3_col5")
      #showElement("button_row")
    } else if(length(row) == 3) {
      showElement("row3_col1")
      showElement("row3_col2")
      showElement("row3_col3")
      hideElement("row3_col4")
      hideElement("row3_col5")
      #showElement("button_row")
    } else if(length(row) == 4) {
      showElement("row3_col1")
      showElement("row3_col2")
      showElement("row3_col3")
      showElement("row3_col4")
      hideElement("row3_col5")
      #showElement("button_row")
    } else if(length(row) == 5) {
      showElement("row3_col1")
      showElement("row3_col2")
      showElement("row3_col3")
      showElement("row3_col4")
      showElement("row3_col5")
      #showElement("button_row")
    }
    for(i in length(row)) {
      if(length(row) == 0) {
        hideElement("row3_col1")
        hideElement("row3_col2")
        hideElement("row3_col3")
        hideElement("row3_col4")
        hideElement("row3_col5")
        #hideElement("button_row")
        break
      }
      if(length(row) > 5) { break }
      name_data = paste("selection.binding_data", i, sep = "")
      name_display = paste("selection.display_table", i, sep = "")
      name_title = paste("selection.title", i, sep = "")
      name_file = paste0("selection.drug", i)
      if(NROW(values$c.data_display_sub) > 0) {
        dt1 = values$c.data_sub
      } else {
        dt1 = values$c.data
      }
      drug = dt1$name_2[ row[i] ]
      hms_id = dt1$hmsID_2[ row[i] ]
      values[[name_file]] = drug
      values[[name_title]] = paste0(hms_id, "; ", drug)
      values$num_selected = length(row)

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
      output_name = paste("selection", i, sep = "")
    }
  }, ignoreInit = T, ignoreNULL = F)

  proxy = dataTableProxy('data_table')

  observeEvent(input$clearButton, {
    proxy %>% selectRows(NULL)
    for(i in 1:5) {
      assign(paste0("values$selection.binding_data",i), NULL)
    }
    values$num_selected = 0
  })

  output$selection1 = renderDataTable(
    values$selection.display_table1,
    rownames = F, options = list(
      dom = 'tp',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )

  output$selection2 = renderDataTable(
    values$selection.display_table2,
    rownames = F, options = list(
      dom = 'tp',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )

  output$selection3 = renderDataTable(
    values$selection.display_table3,
    rownames = F, options = list(
      dom = 'tp',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )
  
  output$selection4 = renderDataTable(
    values$selection.display_table4,
    rownames = F, options = list(
      dom = 'tp',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )
  
  output$selection5 = renderDataTable(
    values$selection.display_table5,
    rownames = F, options = list(
      dom = 'tp',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
        "}"),
      autoWidth = TRUE)
  )

  output$sel1_drug = renderText({ values$selection.title1 })
  output$sel2_drug = renderText({ values$selection.title2 })
  output$sel3_drug = renderText({ values$selection.title3 })
  output$sel4_drug = renderText({ values$selection.title4 })
  output$sel5_drug = renderText({ values$selection.title5 })

  output$downloadBind <- downloadHandler(
    filename = function() {
      return(paste0("BindingData_", format(Sys.time(), "%Y%m%d_%I%M%S"), 
                    ".zip", sep = ""))
    },
    content = function(filename) {
      files_all = list(values$c.binding_data,
                   values$selection.binding_data1,
                   values$selection.binding_data2,
                   values$selection.binding_data3,
                   values$selection.binding_data4,
                   values$selection.binding_data5)
      # take only tables that exist
      print(values$num_selected)
      files = files_all[1:(values$num_selected + 1)]
      drugs = input$query_compound
      if(values$num_selected > 0) {
        for(i in 1:5) {
          drugs = c(drugs, values[[paste0("selection.drug", i)]])
        }
      }
      zipped_csv(files, filename, paste0("BindingData_", drugs), format(Sys.time(), "%Y%m%d_%I%M%S") )
    }, contentType = "application/zip"
  )
  session$allowReconnect(TRUE)
})
