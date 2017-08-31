library(shiny)
library(dplyr)
library(readr)
library(DT)

# load tables
selection_table_selectivity = read_csv("selection_table_selectivity_edited.csv")
selection_table_clindev = read_csv("selection_table_clinical_development.csv")
merge_cmpd_info = read_csv("cmpd_info_library_designer.csv")
merge_table_geneinfo = read_csv("gene_info_library_designer.csv")

selection_table_selectivity$SD_aff = NA

# table for kinase example
kinase_example = read_tsv("kinhub_kinases.tsv")

# Define genes found in our data
all_genes = union(unique(selection_table_clindev$symbol), unique(selection_table_selectivity$symbol))

# Names of classes in the selectivity table
best = c("bestclass_I", "bestclass_II")
second = c("secondclass_I", "secondclass_II")
non = c("non_selective")
un = c("unknown_selectivity_I", "unknown_selectivity_II")
none = NULL
# Names of phases in the clinical development table
approved = "approved"
three = c("max_phase_3")
two = c("max_phase_2")
one = c("max_phase_1")

tab.js = "$('.menu .item')
  .tab()
;"

about.modal.js = "$('.ui.mini.modal')
  .modal('show')
;"

shinyServer(function(input, output, session) {
  runjs(tab.js)
  # Define reactive values
  values = reactiveValues(gene_list = NULL, genes_not_found = NULL, gene_list_found = 0,
                          genes_not_found_paste = NULL, submitted = F,
                          sources = NULL, probes = NULL, clinical = NULL,
                          display_per_cmpd = NULL, display_per_entry = NULL)
  # Make app stop when you close the webpage
  session$onSessionEnded(stopApp)
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # Load example gene set of kinases
  observeEvent(eventExpr = c(input$load_example_kinases, input$load_example_kinases2), handlerExpr = {
    updateTextAreaInput(session, inputId = "gene_list", value = paste0(kinase_example$`HGNC Name`, collapse = "\n"))
  }, ignoreInit = T, ignoreNULL = T)
  
  # Jump to results tab when "Submit" is clicked
  observeEvent(input$submitButton, {
    showElement("tab2_top")
    removeClass(id = "tab1_top", class = "active")
    removeClass(id = "tab1_bottom", class = "active")
    addClass(id = "tab2_top", class = "active")
    addClass(id = "tab2_bottom", class = "active")
  })
  
  # Disable suspend for outputs to fix issue with certain outputs not updating 
  # after "submit" button is pushed... probably an issue with "shiny.semantic"
  output$search_genes = renderText("")
  output$gene_total = renderText("")
  outputOptions(output, "search_genes", suspendWhenHidden = FALSE)
  outputOptions(output, "gene_total", suspendWhenHidden = FALSE)
  

  # Make sure genes given are in the genes that we have info for
  observeEvent(eventExpr = input$gene_list, handlerExpr = {
    values$gene_list = unlist(strsplit(input$gene_list, "\n"))
    values$genes_not_found = values$gene_list[!values$gene_list %in% all_genes]
    values$genes_not_found_paste = paste(values$genes_not_found, collapse = ", ")
    output$search_genes = renderText({ paste(length(values$genes_not_found), "gene(s) not found:", values$genes_not_found_paste) })
    values$gene_list_found = values$gene_list[values$gene_list %in% all_genes]
    output$gene_total = renderText({ paste(length(values$gene_list_found), "gene(s) entered.") })
  })
  
  observeEvent(input$submitButton, {
    values$submitted = T
    # Get probe class selections
    observeEvent(input$probes, {
      print(input$probes)
      values$probes = NULL
      if(!is.null(input$probes)) {
        for(i in 1:length(input$probes)) {
          values$probes = c(values$probes, get(input$probes[i]))
        }
      } else {
        values$probes = NULL
      }
      print(values$probes)
    }, ignoreNULL = F)
    
    # Get clinical phase selections
    observeEvent(input$clinical, {
      print(input$clinical)
      if(!is.null(input$clinical)) {
        values$clinical = NULL
        for(i in 1:length(input$clinical)) {
          values$clinical = c(values$clinical, get(input$clinical[i]))
        }
      } else {
        values$clinical = NULL
      }
      print(values$clinical)
    }, ignoreNULL = F)
    observeEvent(c(input$clinical, input$probes, input$meas, 
                   input$sd, input$affinity, input$legacy), {
      output_selectivity = selection_table_selectivity %>%
        filter(symbol %in% values$gene_list_found) %>%
        filter(source %in% values$probes)
      output_clindev = selection_table_clindev %>%
        filter_(~symbol %in% values$gene_list_found) %>%
        filter_(~source %in% values$clinical) %>%
        # sliders "sd" and "affinity" are in log10 scale
        filter_(~mean_Kd <= 10^input$affinity) %>%
        filter_(~SD_aff <= 10^input$sd | is.na(SD_aff)) %>%
        filter_(~n_measurement >= input$meas)
      output_table = rbind(output_selectivity[c("gene_id","molregno","mean_Kd", "SD_aff",
                                                 "n_measurement","source")],
                            output_clindev[c("gene_id","molregno","mean_Kd", "SD_aff",
                                             "n_measurement","source")])
      print(output_table)
      values$display_per_entry = unique(output_table %>%
        merge(merge_cmpd_info[c("molregno","chembl_id","pref_name","max_phase")],
              by="molregno") %>%
        merge(merge_table_geneinfo,by="gene_id"))
  
      values$display_per_entry = values$display_per_entry[c("symbol","chembl_id",
        "pref_name","source","max_phase","mean_Kd", "SD_aff","n_measurement",
        "gene_id","tax_id")]
      values$display_per_entry = values$display_per_entry %>% mutate(
        symbol = factor(symbol), chembl_id = factor(chembl_id), pref_name = factor(pref_name),
        source = factor(source), gene_id = factor(gene_id), tax_id = factor(tax_id),
        max_phase = as.integer(max_phase)
      ) %>% mutate_at(c("mean_Kd", "SD_aff"), funs(if_else(. > 1, round(.,1), signif(.,2)))) %>%
        rename(`mean_Kd_(nM)` = mean_Kd, `SD_Kd_(nM)` = SD_aff, reason_included = source)
      # rounds mean and SD to closest 0.1 if greater than 1.
      # if less than one, rounds to two significant digits.

      values$display_per_cmpd = unique(output_table %>%
        merge(merge_cmpd_info[c("molregno","chembl_id","pref_name",
          "max_phase","alt_names","inchi")], by="molregno") %>%
        merge(merge_table_geneinfo,by="gene_id")) %>%
        group_by(molregno,chembl_id,pref_name,alt_names,inchi,max_phase) %>%
        summarise(sources=toString(paste0(symbol,";",source))) %>% as.data.frame %>% mutate(
        molregno = factor(molregno), chembl_id = factor(chembl_id), pref_name = factor(pref_name),
        max_phase = as.integer(max_phase)
      ) %>% rename(reason_included = sources)
    }, ignoreNULL = F)
  })

  # Toggle filters when button is clicked
  observeEvent(input$filter_button, {
    toggleElement(id = "filters", anim = T, animType = "fade")
    toggleElement(id = "filter_down")
    toggleElement(id = "filter_right")
  })
  
  # display correct table
  observeEvent(input$table, {
    if(input$table == "entry") {
      output$output_table = DT::renderDataTable({
        datatable(values$display_per_entry, extensions = c('Buttons', 'FixedHeader'),
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
    } else {
      output$output_table = DT::renderDataTable({
        datatable(values$display_per_cmpd, extensions = c('Buttons','FixedHeader'),
                  filter = 'top',
                  rownames = F, options = list(
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'colvis'),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"),
          # hide molregno column
          columnDefs = list(list(visible=FALSE, targets=0)),
          searchHighlight = TRUE,
          fixedHeader = TRUE,
          autoWidth = TRUE
        ))
      })
    }
  })
})