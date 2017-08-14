library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
if (sci) {
// scientific style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
})
} else {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) { return (Math.pow(10, num)); }
})
}
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('affinity', sci = false)

logifySlider('sd', sci = false)
}, 5)})
"

JS.slider <-
  "
// test
$(#affinity).ionRangeSlider({
  grid: true,
  min: 10,
  max: 10000,
  from: 1000,
  values: [10, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]
});
"

shinyUI(
  semanticPage(
    title = "Custom Library App",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    br(),
    # tags$head(tags$script(HTML(JS.logify))),
    # tags$head(tags$script(HTML(JS.onload))),
    #tags$head(tags$script(HTML(JS.slider))),
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
          a(class = "ui red ribbon label", "Maximum clinical phase"),
          div(class = "ui form",
            div(class = "ui raised segment",
  radioButtons("clinical", "", choiceNames = c("None","Approved only","Approved and phase III only", "Approved and phases II and III only", "Approved and phases I, II, and III"), choiceValues = c("none","approved", "three", "two", "one"), selected = "approved"),
  hr(),
  sliderInput(inputId = "affinity", label = "Maximum Kd for query target (nM)",
              min = 10, max = 10000, value = 1000),
  sliderInput(inputId = "meas", label = "Minimum number of measurements",
              min = 1, max = 400, value = 2),
  sliderInput(inputId = "sd", label = "Maximum std. dev. of Kd (nM)",
              min = 10, max = 100000, value = 100)
            )
          )
        ),
        div(class = "column",
          a(class = "ui red ribbon label", "Probes"),
          div(class = "ui form",
              div(class = "ui raised segment",
                  radioButtons("probes", "", choiceNames = c("None","Best class only", "Best class and second class only", "Best class, second class, and non-specific only", "All including compounds with unknown selectivity"), choiceValues = c("none","best", "second", "non", "un"), selected = "best")
              )),
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
  hidden(
          div(class = "ui form", id = "show_output_table",
  radioButtons(inputId = "table", "", choiceNames = c("Display per entry", "Display per compound"),
               choiceValues = c("entry", "cmpd"), inline = T),
  DT::dataTableOutput("output_table")
  )
          )
        )
      )
    )
  )
)