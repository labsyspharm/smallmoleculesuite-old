library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)
library(plotly)
library(readr)
library(d3scatter)

similarity_table = read_csv("input/similarity_table_ChemblV22_1_20170804.csv")
affinity_selectivity = read_csv("input/affinity_selectivity_table_ChemblV22_1_20170804.csv")

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId) {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) {
return (Math.pow(10, num).toLocaleString());
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
logifySlider('sd')
logifySlider('affinity')
}, 5)})
"

shinyUI(
  semanticPage(
    title = "Query Drug App",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    # Fix for mobile viewing
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
  box-sizing: border-box;
              }"),
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
    div(class = "ui mini modal",
      div(class = "actions",
        div(class = "ui red basic circular cancel icon button", uiicon(type = "window close")
        )
      ),
      div(class = "ui center aligned basic segment",
          p("Developed at Harvard Medical School (Sorger lab) by ", "Nienke Moret, ", a("Marc Hafner", href = "https://scholar.harvard.edu/hafner"), ", and ", a("Nicholas Clark", href = "https://github.com/NicholasClark/")),
          h3("Application programming"),
          p("Nienke Moret (R scripts)"),
          p("Nicholas Clark (R/Shiny application)"),
          br(),
          p("[paper citation]"),
          h3("Links"),
          p(a(class = "item", "Sorger lab website", href = "http://sorger.med.harvard.edu")),
          p(a(class = "item", "Project github repository", uiicon("github"), 
            href = "https://github.com/sorgerlab/drug_browser")),
          p(a(class = "item", "LINCS-DCIC website", href = "http://lincs-dcic.org"))
      )
    ),
    div(class = "ui container",
      div(class = "ui top attached inverted five item stackable menu", style = "width: 100%;",
        div(class = "ui center aligned container",
            a(class = "item", img(class = "logo", src = "dcic.png"),
              href = "http://lincs-dcic.org"),
            a(class = "item", "Custom Library App"),
            a(class = "item", "Query Drug App"),
            a(class = "item", "Query Gene App"),
            a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
              href = "http://sorger.med.harvard.edu" )
        )
      ),
      div(class = "ui main container attached segment", style = "margin: 0px;",
        div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
          div(class = "ui grid",
            div(class = "row",
              div(class = "stackable column", style = "width: 300px; min-width: 300px;",
  h3(class="ui horizontal divider header", uiicon("info circle"), "Instructions"),
  p("Select a drug to query by searching in the box below. Adjust the sliders to change the parameters. [ explain what the application does and how the parameters work here ]."),
  selectizeInput('query_compound', 'Select Query Compound', selected = NULL,
                 choices = sort(unique(similarity_table$name_1)), multiple = F,
                 options = list(placeholder = "Select a drug",
                                onInitialize = I('function() { this.setValue(""); }')
                                ))
              ),
              div(class = "stackable column", style = "width: calc(100% - 300px)",
  h3(class="ui horizontal divider header", uiicon("filter"), "Filters"),
                div(class = "ui noshadow horizontal segments",
                  div(class = "ui basic compact segment",
  sliderInput("n_common", "n_assays_common_active value", min = 0, max = 15, step = 1, value = 0),
  sliderInput("n_pheno", "n_pheno_assays_active_common value", min = 0, max = 15, step = 1, value = 0)
                  ),
                  div(class = "ui basic compact segment",
  sliderInput("affinity", "Minimum/maximum affinity", min = -3, max = 10, step = 1, value = c(-3,6)),
  sliderInput("sd", "Maximum std. dev. of affinity", min = 0, max = 10, step = 1, value = 5),
  sliderInput("min_measurements", "min_measurements value", min = 1, max = 15, step = 1, value = 2)
                  )
                )
              )
            ),
            div(class = "row",
  h3(class="ui horizontal divider header", uiicon("bar chart"), "Main plot")
            ),
            div(class = "row",
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader1",
                            "Loading Plot 1"))),
  d3scatterOutput("mainplot1")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader2",
                            "Loading Plot 2"))),
  d3scatterOutput("mainplot2")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader3",
                            "Loading Plot 3"))),
  d3scatterOutput("mainplot3")
              )
            ),
  #           div(class = "row",
  # #verbatimTextOutput("brush"),
  # #verbatimTextOutput("hover")
  #           ),
            div(class = "row",
              div(class = "column", style = "margin-bottom: 50px;",
  h3(class="ui horizontal divider header", uiicon("table"), "Output table"),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    hidden(div(class = "ui active text loader", id = "loader_tab", "Loading Table",
               style = "margin-top: 50px; margin-bottom: 50px;"))),
    DT::dataTableOutput('data_table')
              )
            )
          )
        )
      ),
    div(class = "ui bottom attached inverted footer segment", style = "margin: 0px; width: 100%;",
      div(class = "ui center aligned container",
        div(class = "ui horizontal inverted large divided link list",
  a(class = "item", div(class = "action-button", "About", id = "about") ),
  a(class = "item", "Contact Us"),
  a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/drug_browser")
        )
        )
      )
    )
  )
)