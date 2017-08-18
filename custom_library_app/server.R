library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(DT)

# load tables
selection_table_selectivity = read_csv("selection_table_selectivity_edited.csv")
selection_table_clindev = read_csv("selection_table_clinical_development.csv")
merge_cmpd_info = read_csv("cmpd_info_library_designer.csv")
merge_table_geneinfo = read_csv("gene_info_library_designer.csv")
# table for kinase example
kinase_example = read_tsv("kinhub_kinases.tsv")

# Define genes found in our data
all_genes = union(unique(selection_table_clindev$symbol), unique(selection_table_selectivity$symbol))

# Names of classes in the selectivity table
best = c("bestclass_I", "bestclass_II")
second = c(best, "secondclass_I", "secondclass_II")
non = c(best, second, "non_selective")
un = c(best, second, non, "unknown_selectivity_I", "unknown_selectivity_II")
none = NULL
# Names of phases in the clinical development table
approved = "approved"
three = c(approved, "max_phase_3")
two = c(three, "max_phase_2")
one = c(two, "max_phase_1")

tab.js = "$('.menu .item')
  .tab()
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
  
  # Load example gene set of kinases
  observeEvent(eventExpr = input$load_example_kinases, handlerExpr = {
    updateTextAreaInput(session, inputId = "gene_list", value = paste0(kinase_example$`HGNC Name`, collapse = "\n"))
  })
  
  observeEvent(input$submitButton, {
    removeClass(id = "tab1_top", class = "active")
    removeClass(id = "tab1_bottom", class = "active")
    addClass(id = "tab2_top", class = "active")
    addClass(id = "tab2_bottom", class = "active")
  })

  # Make sure genes given are in the genes that we have info for
  observeEvent(eventExpr = input$gene_list, handlerExpr = {
    values$gene_list = unlist(strsplit(input$gene_list, "\n"))
    values$genes_not_found = values$gene_list[!values$gene_list %in% all_genes]
    values$genes_not_found_paste = paste(values$genes_not_found, collapse = ", ")
    output$search_genes = renderText({ paste(length(values$genes_not_found), "gene(s) not found:", values$genes_not_found_paste) })
    values$gene_list_found = values$gene_list[values$gene_list %in% all_genes]
    output$gene_total = renderText({ paste(length(values$gene_list_found), "gene(s) entered.") })
  })
  
  # Get probe class selections
  observeEvent(input$probes, {
    print(input$probes)
    values$probes = get(input$probes)
    print(values$probes)
  })
  
  # Get clinical phase selections
  observeEvent(input$clinical, {
    print(input$clinical)
    values$clinical = get(input$clinical)
    print(values$clinical)
  })
  
  observeEvent(input$submitButton, {
    values$submitted = T
    observeEvent(c(input$clinical, input$probes, input$meas, 
                   input$sd, input$affinity, input$legacy), {
      output_selectivity = selection_table_selectivity %>%
        filter_(~symbol %in% values$gene_list_found) %>%
        filter_(~source %in% values$probes)
      output_clindev = selection_table_clindev %>%
        filter_(~symbol %in% values$gene_list_found) %>%
        filter_(~source %in% values$clinical) %>%
        filter_(~mean_Kd <= input$affinity) %>%
        filter_(~SD_aff <= input$sd) %>%
        filter_(~n_measurement>= input$meas)
      output_table = rbind(output_selectivity[c("gene_id","molregno","mean_Kd",
                                                 "n_measurement","source")],
                            output_clindev[c("gene_id","molregno","mean_Kd",
                                             "n_measurement","source")])
      
      values$display_per_entry = unique(output_table %>%
        merge(merge_cmpd_info[c("molregno","chembl_id","pref_name","max_phase")],
              by="molregno") %>%
        merge(merge_table_geneinfo,by="gene_id"))
  
      values$display_per_entry = values$display_per_entry[c("symbol","chembl_id",
        "pref_name","source","max_phase","mean_Kd","n_measurement",
        "gene_id","tax_id")]
      
      values$display_per_cmpd = unique(output_table %>%
        merge(merge_cmpd_info[c("molregno","chembl_id","pref_name",
          "max_phase","alt_names","inchi")], by="molregno") %>%
        merge(merge_table_geneinfo,by="gene_id")) %>%
        group_by(molregno,chembl_id,pref_name,alt_names,inchi,max_phase) %>%
        summarise(sources=toString(paste0(symbol,";",source)))
    })
  })

  # Show output table after submit button is clicked
  observeEvent(input$submitButton, {
    showElement(id = "show_output_table", anim = T, animType = "fade")
  })
  
  # display correct table
  observeEvent(input$table, {
    if(input$table == "entry") {
      output$output_table = DT::renderDataTable(values$display_per_entry)
    } else {
      output$output_table = DT::renderDataTable(values$display_per_cmpd)
    }
  })
})