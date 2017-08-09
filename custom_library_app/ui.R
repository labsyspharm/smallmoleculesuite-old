library(shiny)
library(shiny.semantic)
library(shinyjs)

shinyUI(
  semanticPage(
    title = "My page",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    br(),
    div(class="ui container segment",
      div(class = "ui three column centered grid",
        div(class = "column",
          h2("Custom library app")
        )
      ),
      div(class = "ui stackable three column centered grid", 
        div(class = "column",
          a(class = "ui red ribbon label", "Genes/targets"),
          div(class = "ui form",
  textAreaInput(inputId = "gene_list", label = "", value = "", placeholder = "MTOR\nRPS6K1\nAKT\n...\netc.")
            ),
  textOutput("gene_total"),
  textOutput("search_genes"),
          div(class = "ui button red", "Submit")
            ),
        div(class = "column",
          a(class = "ui red ribbon label", "Probes"),
          div(class = "ui form",
            div(class = "ui raised segment",
  checkboxGroupInput("probes", "", choiceNames = c("Best Class", "Second Class", "Non-specific", "Unknown Selectivity"), choiceValues = c("best", "second", "non", "un"))
            )),
            a(class = "ui red ribbon label", "Clinical Contribution"),
          div(class = "ui form",
            div(class = "ui raised segment",
  checkboxGroupInput("clinical", "", choiceNames = c("Approved", "Phase I", "Phase II", "Phase III"), choiceValues = c("approved", "one", "two", "three"))
            )
          )
        ),
        div(class = "column",
          a(class = "ui red ribbon label", "Legacy compounds"),
          div(class = "ui form",
              div(class = "ui raised segment",
  checkboxGroupInput("legacy", "", choiceNames = c("Gray best inhibitor list", "chemicalprobes.org 4.0 star rating"), choiceValues = c("gray", "chem_probe"))
            )
          )
        )
      ),
      div(class = "ui one column centered grid",
          div(class = "column",
              dataTableOutput("output_table")
          )
      )
    )
  )
)