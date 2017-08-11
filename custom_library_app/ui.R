library(shiny)
library(shiny.semantic)
library(shinyjs)

shinyUI(
  semanticPage(
    title = "My page",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    br(),
    tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 5px; background: black; border-top: 0px solid black; border-bottom: 0px solid black;}
               .irs-bar-edge {background: black; border: 0px solid black; height: 5px; width: 10px; border-radius: 0px;}
               .irs-line {border: 0px solid black; height: 5px; border-radius: 0px;}
               .irs-grid-pol {display: none;}
               .irs-grid-text {font-size: 0px;}
               .irs-max {font-family: 'arial'; color: black;}
               .irs-min {font-family: 'arial'; color: black;}
               .irs-single {color:black; background:white; fond-size: 20px;}
               .irs-slider {width: 20px; height: 20px; top: 17px;}
               "),
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
  textAreaInput(inputId = "gene_list", label = "", value = "", placeholder = "MTOR\nRPS6KB1\nAKT\n...\netc.")
            ),
  textOutput("gene_total"),
  textOutput("search_genes"),
          div(class = "ui button red action-button", "Submit", id = "submitButton")
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
  checkboxGroupInput("clinical", "", choiceNames = c("Approved", "Phase I", "Phase II", "Phase III"), choiceValues = c("approved", "max_phase_1", "max_phase_2", "max_phase_3"))
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
      div(class = "ui three column centered grid",
        div(class = "column",
          div(class = "ui form",
            div(class = "ui raised segment",
              div(class = "ui range",
  sliderInput(inputId = "affinity", label = "Maximum mean affinity(nM)",
              min = 0, max = 1000, value = 1000)
              )
            )
          )
        ),
        div(class = "column",
          div(class = "ui form",
            div(class = "ui raised segment",
  sliderInput(inputId = "sd", label = "Maximum affinity std. dev. (nM)",
              min = 0, max = 100, value = 100)
            )
          )
        ),
        div(class = "column",
          div(class = "ui form",
            div(class = "ui raised segment",
  sliderInput(inputId = "meas", label = "Minimum measurement threshold",
              min = 0, max = 10, value = 1)
            )
          )
        )
      ),
      div(class = "ui one column centered grid",
        div(class = "column",
          div(class = "ui form",
  dataTableOutput("output_table")
          )
        )
      )
    )
  )
)