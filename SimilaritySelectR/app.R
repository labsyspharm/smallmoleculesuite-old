library(shiny)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(crosstalk)
library(shiny.semantic)
library(shinyjs)
library(markdown)
library(rclipboard)
library(clipr)
library(aws.s3)

app_name = "SimilaritySelectR"
source(".awspass")

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
similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv") %>%
  mutate_at(c("PFP","TAS","structural_similarity"),
            function(x) round(x, 2))
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv") %>%
  mutate_at(vars(c(`mean_Kd_(nM)`, `SD_Kd_(nM)`:offtarget_IC50_N)),
            function(x) signif(x, 2))
selectivity_order = c("Most selective","Semi-selective","Poly-selective","Unknown","Other")

# Function for toolip values
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

# open about modal
about.modal.js = "$('.ui.mini.modal')
.modal({
blurring: true
})
$('#about_modal').modal('show')
;"
bookmark.modal.js = "$('.ui.mini.modal')
.modal({
blurring: true
})
$('#bookmark_modal').modal('show')
;"

server = function(input, output, session) {
  # Make app stop when you close the webpage
  #session$onSessionEnded(stopApp)
  
  # Set locale so that sorting works correctly
  Sys.setlocale("LC_COLLATE","en_US.UTF-8")
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # Run js to hide warning messages on click
  runjs(message.hide.js)
  
  new_input = NULL

  onRestore(function(state) {
    print("onRestore start")
    query_id = getQueryString()$bookmark
    input_name = paste0("sms_bookmarks/", query_id, "/input.rds")
    if( object_exists(object = input_name, bucket = aws_bucket) ) {
      new_input <<- s3readRDS(object = input_name, bucket = aws_bucket)
    } else {
      showElement(id = "bookmark_not_found")
    }
    print("onRestore end")
  })
  
  onRestored(function(state) {
    print("onRestored start")
    ### restore the state if the bookmark is found
    if(!is.null(new_input)) {
      ## select the compound
      updateSelectizeInput(session, "query_compound", selected = new_input$query_compound)
      ## update sliders
      updateSliderInput(session, inputId = "n_common", value = new_input$n_common)
      updateSliderInput(session, inputId = "n_pheno", value = new_input$n_pheno)
      ## show/hide elements
      if(floor(new_input$filter_button/2) != new_input$filter_button/2) { shinyjs::click("filter_button") }
      if(floor(new_input$intro_hide/2) != new_input$intro_hide/2) { shinyjs::click("intro_hide") }
      ## restore other values
      values$points_selected1 = new_input$points_selected1
      values$points_selected2 = new_input$points_selected2
      values$points_selected3 = new_input$points_selected3
      values$rows_selected_save = new_input$output_table_rows_selected
      ## reset saved input placeholder object
      new_input <<- NULL
      #values$bookmark_restart = T
    }
    #updateQueryString("?")
    print("onRestored end")
  })
  
  onBookmark(function(state) {
    print("bookmark")
    if(exists("d")) {
      if(length(d$selection(ownerId = "mainplot1")) > 0) {
        values$points_selected1 = d$selection(ownerId = "mainplot1") %>% which()
        values$points_selected2 = d$selection(ownerId = "mainplot2") %>% which()
        values$points_selected3 = d$selection(ownerId = "mainplot3") %>% which()
        #values$groupId = d$groupName()
      }
    }
  })
  
  onBookmarked(function(url) {
    print("bookmarked")
    date_time = format(Sys.time(), "%Y%m%d-%H%M%S")
    id = substr(as.character(runif(1)), 3, 6)
    new_id = paste0(app_name, "-", date_time, "-", id)
    new_url = gsub("\\?_inputs_.*", paste0("?bookmark=",new_id), url)
    session$sendCustomMessage("bookmark_url", message = new_url)
    values$url = new_url
    input_list = reactiveValuesToList(input, all.names = T)
    print("input_list")
    print(names(input_list))
    #row_sel = grep("_rows_selected$", names(input_list), value = T)
    input_list_save = input_list[c("query_compound", "filter_button", "n_pheno", "intro_hide", "n_common",
                                   "output_table_rows_selected")]
    #values_list = reactiveValuesToList(values, all.names = T)
    #for(x in row_sel) { values_list[[x]] = input_list[[x]] }
    #points_selected_names = grep("^points_selected[1-3]", names(values_list), value = T)
    #selected_drug_names = grep("^selection.drug[1-5]", names(values_list), value = T)
    input_list_save$points_selected1 = values$points_selected1
    input_list_save$points_selected2 = values$points_selected2
    input_list_save$points_selected3 = values$points_selected3
    s3saveRDS(input_list_save, bucket = aws_bucket, object = paste0("sms_bookmarks/", new_id, "/", "input.rds"))
    updateQueryString(new_url)
  })
  
  ##### For updating URL query string
  # observe({
  #   # Needed to call input to trigger bookmark
  #   all_vars = reactiveValuesToList(input, all.names = T)
  #   # Don't delete above line -- needed for point selection bookmarking
  #   session$doBookmark()
  # })
  
  observeEvent(input$bookmark1, {
    runjs(bookmark.modal.js)
  })
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy", values$url, icon("clipboard"))
  })
  
  # Workaround for execution within RStudio
  #observeEvent(input$clipbtn, clipr::write_clip(values$url))
  
  
  observeEvent(values$c.binding_display, {
    print(values$c.binding_display)
    
    # Binding table output
    output$binding_data = renderDataTable(
      if(dim(values$c.binding_display)[1] > 0) {
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
  observeEvent(c(input$query_compound, input$n_common, input$n_pheno), {
    if(!is.null(input$query_compound) & input$query_compound != "") {
      print("main loop")
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
        filter(name_1 == input$query_compound) %>%
        filter(n_biol_assays_common_active >= 2^input$n_common | is.na(n_biol_assays_common_active)) %>%
        filter(n_pheno_assays_active_common >= 2^input$n_pheno | is.na(n_pheno_assays_active_common)) %>%
        mutate_at(vars(PFP), funs(ifelse(is.na(PFP), -1.1, PFP))) %>%
        mutate_at(vars(TAS), funs(ifelse(is.na(TAS), -0.1, TAS))) %>%
        mutate_at(vars(structural_similarity), funs(ifelse(is.na(structural_similarity), -0.1, structural_similarity)))
      
      values$c.data_display = values$c.data

      values$c.data_title = paste0(unique(values$c.data$hmsID_1),";",unique(values$c.data$name_1))
      
      ## show affinity data of reference compound+ selected compounds
      # filter by name or hms id?
      values$c.binding_data = affinity_selectivity %>% filter(name == input$query_compound) %>%
        #filter(`mean_Kd_(nM)` >= 10^input$affinity[1]) %>%
        #filter(`mean_Kd_(nM)` <= 10^input$affinity[2]) %>%
        #filter(`SD_Kd_(nM)` <= 10^input$sd) %>%
        #filter(n_measurements >= input$min_measurements) %>%
        mutate(selectivity_class = factor(selectivity_class,levels=selectivity_order)) %>%
        arrange(selectivity_class, `mean_Kd_(nM)`)
      
      d <<- SharedData$new(values$c.data, ~name_2)
      
      values$ref_hms_id = unique(values$c.binding_data$hms_id)
      
      values$c.binding_display = values$c.binding_data[,c(3,4,5)]
      
      points1 = values$points_selected1
      points2 = values$points_selected2
      points3 = values$points_selected3
      
      output$mainplot1 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~PFP, mode = "markers", 
                  color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                    name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, "\ny: ", 
                                                                    PFP, sep = ""), hoverinfo = "text") %>%
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
                              title = "Phenotypic Correlation",
                              tickmode = "array",
                              tickvals = c(-1.1, seq(-1,1,.5)),
                              ticktext = c("NA", as.character(seq(-1,1,.5))) )
          ) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2), hoverinfo = "text")
        # if restoring from a bookmark, select previously selected points
        p$x$highlight$defaultValues = values$c.data$name_2[points1]
        p$x$highlight$color = "rgba(255,0,0,1)"
        p$x$highlight$off = "plotly_deselect"
        p %>% layout(dragmode = "select")
      })
      if(sum(values$points_selected1) > 0) {
        d$selection(points1, ownerId = "mainplot1")
        values$points_selected1 = F
      }
      
      output$mainplot2 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~structural_similarity, y = ~TAS, mode = "markers", 
                  color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                    name_1, "\nDrug 2: ", name_2, "\nx: ", structural_similarity, 
                                                                    "\ny: ", TAS, sep = ""), hoverinfo = "text") %>%
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
                              title = "Target Similarity",
                              tickmode = "array",
                              tickvals = c(-0.15, seq(0,1,.2)),
                              ticktext = c("NA", as.character(seq(0,1,.2))) )) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
        # if restoring from a bookmark, select previously selected points
        p$x$highlight$defaultValues = values$c.data$name_2[points2]
        p$x$highlight$color = "rgba(255,0,0,1)"
        p$x$highlight$off = "plotly_deselect"
        p %>% layout(dragmode = "select")
      })
      if(sum(values$points_selected2) > 0) {
        d$selection(points2, ownerId = "mainplot2")
        values$points_selected2 = F
      }
      
      output$mainplot3 <- renderPlotly({
        p <- d %>%
          plot_ly(x = ~TAS, y = ~PFP, mode = "markers", 
                  color = I('black'), name = ~name_2, text = ~paste("Drug 1: ", 
                                                                    name_1, "\nDrug 2: ", name_2, "\nx: ", TAS, "\ny: ", PFP, sep = ""),
                  hoverinfo = "text") %>%
          layout(showlegend = F,
                 shapes = list(list(type='line', x0= -0.1, x1= -0.1, y0=-1.2, y1=1.2,
                                    line=list(dash='dot', width=2, color = "red")),
                               list(type='line', x0= -0.15, x1= 1.15, y0=-1.1, y1=-1.1,
                                    line=list(dash='dot', width=2, color = "red"))),
                 xaxis = list(range = c(-0.15, 1.15),
                              title = "Target Similarity",
                              tickmode = "array",
                              tickvals = c(-0.15, seq(0,1,.25)),
                              ticktext = c("NA", as.character(seq(0,1,.25))) ),
                 yaxis = list(range = c(-1.2, 1.2),
                              title = "Phenotypic Correlation",
                              tickmode = "array",
                              tickvals = c(-1.2, seq(-1,1,.5)),
                              ticktext = c("NA", as.character(seq(-1,1,.5))))) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = ~name_2))
        # if restoring from a bookmark, select previously selected points
        p$x$highlight$defaultValues = values$c.data$name_2[points3]
        p$x$highlight$color = "rgba(255,0,0,1)"
        p$x$highlight$off = "plotly_deselect"
        p %>% layout(dragmode = "select")
      })
      
      if(sum(values$points_selected3) > 0) {
        d$selection(points3, ownerId = "mainplot3")
        values$points_selected3 = F
      }
      
      output$output_table = renderDataTable( {
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
        stateSave = TRUE,
        buttons = c('copy', 'csv', 'excel', 'colvis'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
          "}"),
        searchHighlight = TRUE,
        autoWidth = TRUE), server = T
      )
    }
  }, ignoreInit = T, ignoreNULL = T)
  
  ## On bookmark restart: load row selections in output table and binding table
  # observeEvent(values$bookmark_restart, {
  #   print("start select rows")
  #   if(values$bookmark_restart) {
  #     if(length(values$output_table_rows_selected) > 0) {
  #       print("select rows: output table")
  #       rows = values$output_table_rows_selected
  #       proxy %>% selectRows(rows)
  #     }
  #     if(length(values$binding_data_rows_selected) > 0) {
  #       print("select rows: output table")
  #       rows = values$binding_data_rows_selected
  #       proxy_bind %>% selectRows(rows)
  #     }
  #     values$bookmark_restart2 = T
  #   }
  #   
  # }, ignoreNULL = T, ignoreInit = T, autoDestroy = T)
  
  ## On bookmark restart: load row selections in selection tables
  # observeEvent(values$bookmark_restart2, {
  #   if(values$bookmark_restart2) {
  #     for(i in 1:5) {
  #       var_name = paste0("selection", i, "_rows_selected")
  #       if(length(values[[var_name]]) > 0) {
  #         print(paste0("select rows: ", "selection", i))
  #         rows = values[[var_name]]
  #         get(paste0("proxy", i)) %>% selectRows(rows)
  #       }
  #     }
  #   }
  # }, ignoreInit = T, ignoreNULL = T, autoDestroy = T)
  
  # Make other tables on row selection
  observeEvent(input$output_table_rows_selected, {
    print("rows selected")
    showElement("result_row3")
    showElement("row3_bind_data")
    row = input$output_table_rows_selected
    
    print("start select rows")
    # If restoring bookmarked session, select same rows as before
    if(length(values$rows_selected_save) > 0) {
      print("restore selections")
      row = values$rows_selected_save
      proxy %>%  selectRows(row)
      values$rows_selected_save = NULL
    }

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
    for(i in 1:length(row)) {
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
        arrange(selectivity_class, `mean_Kd_(nM)`)
      
      values[[name_display]] = values[[name_data]][,c(3,4,5)]
      output_name = paste("selection", i, sep = "")
    }
    # if(values$bookmark_restart2) {
    #   for(i in 1:5) {
    #     var_name = paste0("selection", i, "_rows_selected")
    #     if(length(values[[var_name]]) > 0) {
    #       print(paste0("select rows: ", "selection", i))
    #       rows = values[[var_name]]
    #       get(paste0("proxy", i), .GlobalEnv) %>% selectRows(rows)
    #     }
    #   }
    #   rows = values$selection1_rows_selected
    #   proxy1 %>% selectRows(rows)
    # }
  }, ignoreInit = T, ignoreNULL = F)
  
  proxy <<- dataTableProxy('output_table')
  proxy1 <<- dataTableProxy('selection1')
  proxy2 <<- dataTableProxy('selection2')
  proxy3 <<- dataTableProxy('selection3')
  proxy4 <<- dataTableProxy('selection4')
  proxy5 <<- dataTableProxy('selection5')
  proxy_bind <<- dataTableProxy('binding_data')
  
  
  # observe({
  #   print("start select rows")
  #   print(values$output_table_rows_selected)
  #   if(length(values$output_table_rows_selected) > 0) {
  #     print("select rows: output table")
  #     proxy %>% selectRows(values$output_table_rows_selected)
  #   }
  #   for(i in 1:5) {
  #     var_name = paste0("selection", i, "_rows_selected")
  #     if(length(values[[var_name]]) > 0) {
  #       print(paste0("select rows: ", "selection", i))
  #       get(paste0("proxy", i)) %>% selectRows(values[[var_name]])
  #     }
  #   }
  #   if(length(values$binding_data_rows_selected) > 0) {
  #     print("select rows: output table")
  #     proxy_bind %>% selectRows(values$binding_data_rows_selected)
  #   }
  # })
  
  observeEvent(input$clearButton, {
    proxy %>% selectRows(NULL)
    for(i in 1:5) {
      assign(paste0("values$selection.binding_data",i), NULL)
    }
    values$num_selected = 0
  })
  
  observe({
    print("render selection tables")
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
    
  })
    
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
}


#### UI

#similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
#affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")

message.hide.js = "$('.message .close')
.on('click', function() {
  $(this)
  .closest('.message')
  .transition('fade')
  ;
})
;"

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId) {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) {
return (Math.pow(2, num).toLocaleString());
}
})
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
//logifySlider('sd')
//logifySlider('affinity')
logifySlider('n_pheno')
logifySlider('n_common')
}, 5)})
"

ui <- function(request) {
  semanticPage(
    title = "HMS-LINCS Small Molecule Suite - SimilaritySelectR",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    tags$head(rclipboardSetup()),
    # Fix for mobile viewing
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
              box-sizing: border-box;
              }
              @media only screen and (min-width:768px) and (max-width:991px){
              .ui.segment{
              border : 0;
              }
              }
              "),
    tags$style(type = "text/css", "
               .irs-bar {width: 100%; height: 5px; background: black; border-top: 0px solid black; border-bottom: 0px solid black;}
               .irs-bar-edge {background: black; border: 0px solid black; height: 5px; width: 10px; border-radius: 0px;}
               .irs-line {border: 0px solid black; height: 5px; border-radius: 0px;}
               .irs-grid-text {font-family: 'arial'; font-size: 10px;}
               .irs-from {font-family: 'arial'; background:white; color: black;}
               .irs-to {font-family: 'arial'; background:white; color: black;}
               .irs-max {font-family: 'arial'; color: black;}
               .irs-min {font-family: 'arial'; color: black;}
               .irs-single {font-family: 'arial'; color:black; background:white;}
               .irs-slider {width: 20px; height: 20px; top: 17px;}
               "),
    # CSS for hiding border on horizontal segments and making them fixed width
    tags$style(type = "text/css", "
               .ui.noshadow.segments {
               box-shadow: none;
               border: none;
               margin-top: 0px;
               margin-bottom: 0px;
               }"
    ),
    singleton(
      tags$head(tags$script('Shiny.addCustomMessageHandler("bookmark_url",
                            function(message) {
                            document.getElementById("bookmark_text").value = message;
                            }
      );'))
    ),
    div(class = "ui mini modal", id = "about_modal",
        div(class = "actions",
            div(class = "ui red basic circular cancel icon button", uiicon(type = "window close")
            )
        ),
        div(class = "ui center aligned basic segment",
            includeMarkdown("www/about.md")
        )
    ),
    div(class = "ui mini modal", id = "bookmark_modal", style = "width: 450px; hposition: absolute; left: 50%; margin-left: -225px;",
        # div(class = "actions",
        #     div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
        # ),
        div(class = "ui center aligned basic segment",
            div(class = "ui form",
                div(class = "field",
                    tags$label("Sharing URL:"),
                    tags$input(type = "text", id = "bookmark_text")
                ),
                # UI ouputs for the copy-to-clipboard buttons
                uiOutput("clip", inline = T)
            )
        )
    ),
    div(class = "ui container", style = "margin: 0px;",
        div(class = "ui top attached inverted five item stackable menu", style = "width: 100%;",
            div(class = "ui center aligned container",
                a(class = "item", img(class = "logo", src = "dcic.png"),
                  href = "http://lincs-dcic.org/"),
                a(class = "item", "SelectivitySelectR", href = "/SelectivitySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "SimilaritySelectR", href = "/SimilaritySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "LibraryR", href = "/LibraryR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
                  href = "http://sorger.med.harvard.edu" )
            )
        ),
        div(class = "ui main container attached segment", style = "margin: 0px; padding-left: 0px;",
            div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
                div(class = "ui stackable grid",
                    div(class = "row",
                        div(class = "stackable column", style = "width: 300px; min-width: 300px;",
                            div(class = "ui basic center aligned segment",
                                h4(class="ui header", "Select reference compound"),
                                selectizeInput(inputId = "query_compound", label = "", choices = sort(unique(similarity_table$name_1)),
                                               options = list(
                                                 placeholder = 'Search for a compound',
                                                 onInitialize = I('function() { this.setValue(""); }')
                                               )
                                ),
                                hidden(div(class = "ui negative message", id = "bookmark_not_found",
                                    tags$i(class = "close icon"),
                                    div(class = "header",
                                        "This bookmark was not found!"
                                        ),
                                    "Check that the URL was entered correctly. If the bookmark is old, it may not work with our current database."
                                    ))
                                #uiOutput("drug_search")
                            )
                        ),
                        div(class = "stackable column", style = "width: calc(100% - 300px); min-width: calc(100% - 300px);",
                            div(class = "ui basic segment", style = "font-size: medium;",
                                h3(class="ui horizontal divider header",
                                   div(class = "item action-button shiny-bound-input", id = "intro_hide",
                                       a(class = "action-button", p(uiicon("caret down", id = "caret_down"),
                                                                    hidden(uiicon(type = "caret right", id = "caret_right")),
                                                                    "Instructions", uiicon(type = "info_circle")), href = "#")
                                   )
                                ),
                                div(id = "intro",
                                    includeMarkdown("www/intro.md")
                                )
                            )
                        ),
                        tags$style(type='text/css', "#steps { font-size: medium; }"),
                        div(class = "stackable column", style = "width: 350px",
                            tags$style(type='text/css', "#col0 { min-width: 100px; width: 100px; border-left: 0px;
                                       border-right: 0px;}"),
                            tags$style(type='text/css', "#col1 { border-left: 0px; border-right: 0px;}")
                            )
                    ),
                    hidden(div(class = "row", id = "result_row1", style = "margin: 0px; padding: 0px",
                               div(class = "ui basic center aligned segment",
                                   h3(class="ui horizontal divider header", uiicon("bar chart"), "Compound similarity plots"),
                                   h5("Select an area of similarity you are interested in. ", intToUtf8(160),intToUtf8(160), " Hover over points for more information. ", intToUtf8(160),intToUtf8(160), " Double-click on plot to un-select region.",
                                      style = "margin: 0px; padding: 0px"),
                                   h3(class = "ui centered header", textOutput("ref_drug"))
                               ))),
                    hidden(div(class = "row", id = "result_row2",
                               div(class = "stackable five wide column",
                                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                    hidden(div(class = "ui active text loader", id = "loader1",
                                                               "Loading Plot 1"))),
                                   plotlyOutput("mainplot1")
                               ),
                               div(class = "stackable five wide column",
                                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                    hidden(div(class = "ui active text loader", id = "loader2",
                                                               "Loading Plot 2"))),
                                   plotlyOutput("mainplot2")
                               ),
                               div(class = "stackable five wide column",
                                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                    hidden(div(class = "ui active text loader", id = "loader3",
                                                               "Loading Plot 3"))),
                                   plotlyOutput("mainplot3")
                               )
                    )),
                    hidden(div(class = "row", style = "margin: 0px; padding: 0px;", id = "filters_head",
                               div(class = "ui basic center aligned segment",
                                   h3(class="ui horizontal divider header",
                                      div(class = "item action-button shiny-bound-input", id = "filter_button",
                                          a(class = "action-button", p(uiicon("caret down", id = "caret_down_fil"),
                                                                       hidden(uiicon(type = "caret right", id = "caret_right_fil")),
                                                                       "Set similarity thresholds", uiicon(type = "filter")), href = "#")
                                      )
                                   )
                               )
                    )),
                    hidden(div(class = "row", style = "margin: 0px; padding: 0px;",
                               id = "filters",
                               div(class = "four wide column"),
                               div(class = "four wide column",
                                   sliderInput("n_common", "Number of biological assays in common with reference compound
                                               ", min = 0, max = 8, step = 1, value = 0)
                                   ),
                               div(class = "four wide column",
                                   sliderInput("n_pheno", "Number of phenotypic assays in common with reference compound
                                               ", min = 0, max = 8, step = 1, value = 0)
                                   )
                               )),
                    tags$style(type = "text/css", "#row3_bind_data { min-width: 375px; }"),
                    hidden(div(class = "row", id = "result_row4",
                               div(class = "column", style = "min-height: 200px;",
                                   h3(class="ui horizontal divider header", uiicon("table"), "Compound similarity data"),
                                   h4(class = "ui centered header", "Select up to three similar compounds for which target affinity information will be displayed"),
                                   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                    hidden(div(class = "ui active text loader", id = "loader_tab", "Loading Table",style = "margin-top: 50px; margin-bottom: 50px;"))),
                                   DT::dataTableOutput('output_table'),
                                   tags$style(type='text/css', "#output_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
                               )
                    )),
                    hidden(div(class = "row", id = "result_row3",
                               div(class = "stackable center aligned column", id = "row3_bind_data", style = "min-width: 390px; margin-right: 5px;",
                                   h3(class="ui horizontal divider header", uiicon("table"), "Reference compound"),
                                   tags$style(type='text/css', "#binding_data { white-space: nowrap; text-overflow: ellipsis; overflow: scroll; min-width: 390px;}"),
                                   tags$style(type = "text/css", "#row3_col1 { width: calc((100% - 400px)/2); min-width: 350px; margin-left: 5px;}"),
                                   tags$style(type = "text/css", "#row3_col2 { width: calc((100% - 400px)/2); min-width: 350px; }"),
                                   tags$style(type = "text/css", "#row3_col3 { width: calc((100%/3) - 10px); min-width: 350px; margin-left:5px;}"),
                                   tags$style(type = "text/css", "#row3_col4 { width: calc((100%/3) - 10px); min-width: 350px; margin-right:5px;}"),
                                   tags$style(type = "text/css", "#row3_col5 { width: calc((100%/3) - 10px); min-width: 350px; margin:5px;}"),
                                   tags$style(type='text/css', "#sel_drug1 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                                   tags$style(type='text/css', "#sel_drug2 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                                   tags$style(type='text/css', "#sel_drug3 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                                   tags$style(type='text/css', "#sel_drug4 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                                   tags$style(type='text/css', "#sel_drug5 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                                   h4(class = "ui centered header", textOutput("binding_drug", inline = T)),
                                   DT::dataTableOutput("binding_data"),
                                   br(), br()
                               ),
                               hidden(div(class = "padded stackable column", id = "row3_col1",
                                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 1"), 
                                          h4(class = "ui centered header", textOutput("sel1_drug")),
                                          DT::dataTableOutput("selection1", width = "250px")
                               )),
                               hidden(div(class = "stackable column", id = "row3_col2",
                                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 2"),
                                          h4(class = "ui centered header", textOutput("sel2_drug")),
                                          DT::dataTableOutput("selection2", width = "250px")
                               )),
                               hidden(div(class = "stackable column", id = "row3_col3",
                                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 3"),
                                          h4(class = "ui centered header", textOutput("sel3_drug")),
                                          DT::dataTableOutput("selection3", width = "250px")
                               )),
                               hidden(div(class = "stackable column",  id = "row3_col4",
                                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 4"),
                                          h4(class = "ui centered header", textOutput("sel4_drug")),
                                          DT::dataTableOutput("selection4", width = "250px")
                               )),
                               hidden(div(class = "stackable column", id = "row3_col5",
                                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 5"),
                                          h4(class = "ui centered header", textOutput("sel5_drug")),
                                          DT::dataTableOutput("selection5", width = "250px")
                               ))
                    )),
                    hidden(div(class = "row", id = "button_row",
                               div(class = "ui secondary button",
                                   downloadLink("downloadBind", "Download binding data (.csv)", style = "color: white;")
                               ),
                               div(class = "ui secondary button action-button", "Clear selections", id = "clearButton"),
                               div(class = "ui grey button action-button shiny-bound-input", id = "bookmark1", "Bookmark...", uiicon("linkify")
                               )
                    ))
                               )
        )
    ),
    div(class = "ui bottom attached inverted footer segment", style = "margin: 0px; width: 100%;",
        div(class = "ui center aligned container",
            div(class = "ui horizontal inverted large divided link list",
                a(class = "item", div(class = "action-button", "About", id = "about") ),
                a(class = "item", "Contact Us"),
                a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/smallmoleculesuite")
            )
        )
    )
        )
    )
}

shinyApp(ui, server, enableBookmarking = "url")
