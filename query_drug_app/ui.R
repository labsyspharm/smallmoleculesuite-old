library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)
library(plotly)
library(readr)
library(ggvis)

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
            a(class = "item", "Custom Library App", href = "http://shiny.ilincs.org/custom_library_app/"),
            a(class = "item", "Query Drug App", href = "http://shiny.ilincs.org/query_drug_app/"),
            a(class = "item", "Query Gene App", href = "http://shiny.ilincs.org/query_gene_app/"),
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
  uiOutput("drug_search")
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
            hidden(div(class = "row", id = "result_row1",
  h3(class="ui horizontal divider header", uiicon("bar chart"), "Main plot")
            )),
            hidden(div(class = "row", style = "height: 350px", id = "result_row2",
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader1",
                            "Loading Plot 1"))),
  ggvisOutput("mainplot1")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader2",
                            "Loading Plot 2"))),
  ggvisOutput("mainplot2")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader3",
                            "Loading Plot 3"))),
  ggvisOutput("mainplot3")
              )
            )),
  tags$style(type = "text/css", "#row3_col1 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_col2 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_col3 { width: 320px; min-width: 320px; }"),
  tags$style(type = "text/css", "#row3_bind_data { width: 375px; min-width: 375px; }"),
            hidden(div(class = "row", id = "result_row3",
             div(class = "stackable column", id = "row3_bind_data",
   h3(class="ui horizontal divider header", uiicon("table"), "Binding data"),
   tags$style(type='text/css', "#binding_data { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug1 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug2 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug3 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
                 DT::dataTableOutput("binding_data")
             ),
              hidden(div(class = "padded stackable column", id = "row3_col1",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 1"), 
  h4(class = "ui centered header", textOutput("sel1_drug")),
  DT::dataTableOutput("selection1")
              )),
              hidden(div(class = "stackable column", id = "row3_col2",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 2"),
  h4(class = "ui centered header", textOutput("sel2_drug")),
  DT::dataTableOutput("selection2")
              )),
              hidden(div(class = "stackable column", id = "row3_col3",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 3"),
  h4(class = "ui centered header", textOutput("sel3_drug")),
  DT::dataTableOutput("selection3")
              ))
            )),
            hidden(div(class = "row", id = "button_row",
  div(class = "ui secondary button action-button", "Clear selections", id = "clearButton")
            )),
            hidden(div(class = "row", id = "result_row4",
              div(class = "column", style = "min-height: 200px;",
  h3(class="ui horizontal divider header", uiicon("table"), "Output table"),
  h4(class = "ui centered header", "Select rows below to see drug targets and binding affinities for a given drug."),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
  hidden(div(class = "ui active text loader", id = "loader_tab", "Loading Table",
             style = "margin-top: 50px; margin-bottom: 50px;"))),
  DT::dataTableOutput('data_table'),
  tags$style(type='text/css', "#data_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
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
  a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/drug_browser")
        )
        )
      )
    )
  )
)