library(shiny)
library(shiny.semantic)
library(shinyjs)

shinyUI(
  semanticPage(
    title = "My page",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    div(class = "ui form", label = "Gene list",
        textAreaInput(inputId = "gene_list", label = "Gene list", value = "", placeholder = "MTOR\nRPS6K1\nAKT")
        ),
    textOutput("gene_total"),
    textOutput("search_genes"),
    div(class = "ui button red", "Submit")
  )
)