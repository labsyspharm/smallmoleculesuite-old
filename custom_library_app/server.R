library(shiny)
library(plotly)
library(dplyr)
library(readr)

library_table = read_csv("selection_table_cmpds_best_and_second_class.csv")
all_genes = unique(library_table$gene_id)
print(head(library_table))

shinyServer(function(input, output, session) {
  # Define reactive values
  values = reactiveValues(gene_list = NULL, genes_not_found = NULL, gene_list_found = 0,
                          genes_not_found_paste = NULL)

  # Make sure genes given are in the genes that we have info for
  observeEvent(eventExpr = input$gene_list, handlerExpr = {
    values$gene_list = unlist(strsplit(input$gene_list, "\n"))
    values$genes_not_found = values$gene_list[!values$gene_list %in% all_genes]
    values$genes_not_found_paste = paste(values$genes_not_found, collapse = ", ")
    output$search_genes = renderText({ paste(length(values$genes_not_found), "gene(s) not found:", values$genes_not_found_paste) })
    values$gene_list_found = values$gene_list[values$gene_list %in% all_genes]
    output$gene_total = renderText({ paste(length(values$gene_list_found), "gene(s) entered.") })
  })
  
  # Output table: (change later)
  output$output_table = renderDataTable(library_table)

})