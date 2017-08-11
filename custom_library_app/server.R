library(shiny)
library(plotly)
library(dplyr)
library(readr)

selection_table_selectivity = read_csv("selection_table_cmpds_best_and_second_class_edited.csv")
selection_table_clindev = read_csv("selection_table_clinical_development.csv")
all_genes = intersect(unique(selection_table_clindev$symbol), unique(selection_table_selectivity$symbol))
best = c("bestclass_I", "bestclass_II")
non = c("non_selective")
second = c("secondclass_I", "secondclass_II")
un = c("unknown_selectivity_I", "unknown_selectivity_II")

shinyServer(function(input, output, session) {
  # Define reactive values
  values = reactiveValues(gene_list = NULL, genes_not_found = NULL, gene_list_found = 0,
                          genes_not_found_paste = NULL, output_table = NULL,
                          sources = NULL, probes = NULL)

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
    values$probes = NULL
    for(i in 1:length(input$probes)) {
      values$probes = c(values$probes, get(input$probes[i]))
    }
    print(values$probes)
  })
  
  observeEvent(input$submitButton, {
    output_selectivity = selection_table_selectivity %>%
      filter_(~symbol %in% values$gene_list_found) %>%
      filter_(~source %in% values$probes)
    output_clindev = selection_table_clindev %>%
      filter_(~symbol %in% values$gene_list_found) %>%
      filter_(~source %in% input$clinical) %>%
      filter_(~mean_aff <= input$affinity) %>%
      filter_(~SD_aff <= input$sd) %>%
      filter_(~n_measurement>= input$meas)
    values$output_table = rbind(output_selectivity[c("gene_id","symbol","molregno","source")],
                                output_clindev[c("gene_id","symbol","molregno","source")])
  })

  output$output_table = renderDataTable(values$output_table)

})