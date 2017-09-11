library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)

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
    title = "Custom Library App",
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
    # CSS for slider styling
    tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 5px; background: black; border-top: 0px solid black; border-bottom: 0px solid black;}
               .irs-bar-edge {background: black; border: 0px solid black; height: 5px; width: 10px; border-radius: 0px;}
               .irs-line {border: 0px solid black; height: 5px; border-radius: 0px;}
               .irs-grid-text {font-family: 'arial'; font-size: 10px;}
               .irs-max {font-family: 'arial'; color: black;}
               .irs-min {font-family: 'arial'; color: black;}
               .irs-single {font-family: 'arial'; color:black; background:white;}
               .irs-slider {width: 20px; height: 20px; top: 17px;}
               "),
    # CSS for hiding border on horizontal segments
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
      div(class = "ui top attached inverted five item stackable menu",
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
      div(class = "ui main attached segment", style = "margin: 0px;",
        div(class="ui top secondary pointing menu", id = "tabs",
          a(class="item active", `data-tab`="tab1", "Gene input", id = "tab1_top"),
          hidden( a(class="item", `data-tab`="tab2", "Results", id = "tab2_top") )
        ),
        div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
          div(class = "ui stackable two column centered grid", 
            div(class = "column", id = "col_input",
  h4(class="ui header", "Type/paste gene symbols below", uiicon("paste")),
  a(class = "ui red ribbon label", "Genes (drug targets)"),
                div(class = "ui form",
  textAreaInput(inputId = "gene_list", label = "", value = "", placeholder = "MTOR\r\nRPS6KB1\r\nAKT\r\n...\r\netc.")
            ),
  br(),
  div(class = "ui button red action-button", "Submit", id = "submitButton"),
  br(),
  textOutput("gene_total"),
  textOutput("search_genes"),
    h4(class="ui horizontal divider header",
       div(class = "item action-button shiny-bound-input", id = "load_example_kinases",
       a(class = "action-button", "Or load example", uiicon("lightning"), href = "#")))
            ),
  tags$style(type='text/css', "#col_input { min-width: 300px; width: 300px;}"),
  tags$style(type='text/css', "#instructions { font-size: medium; padding: 0px; margin: 0px;}"),
            div(class = "column", style = "width: calc(100% - 300px);",
              div(class = "ui form",
  h3(class="ui horizontal divider header", uiicon("info circle"), "Instructions"),
                div(class = "ui container segment basic", id = "instructions",
  p("Type/paste a list of gene symbols into the text box (or load ",
       a(class = "action-button", "our example gene list", href = "#", id = "load_example_kinases2"), ") and click 'Submit'."),
  p("Only gene symbols from ", a("HUGO Gene Nomenclature Committee (HGNC)", href = "http://www.genenames.org/"),
    " are accepted. Non-HGNC gene symbols and genes for which we lack drug information will be ignored."),
  p("After submitting your gene list, a downloadable table of drugs targeting those genes will be generated. You may further filter these drugs by selectivity level, FDA approval/clinical phase, and other parameters.")
                )
              )
            )
          )
        ),
        div(class="ui bottom tab basic segment", `data-tab`="tab2", id = "tab2_bottom",
            style = "padding: 0px;",
          div(class = "ui basic segment", style = "padding: 0px;",
            h4(class="ui horizontal divider header",
              div(class = "item action-button shiny-bound-input", id = "filter_button",
                a(class = "action-button", p(uiicon("caret down", id = "filter_right"),
                  hidden(uiicon(type = "caret right", id = "filter_down")),
                  "Show/hide filters", uiicon(type = "filter")), href = "#")
              )
            ),
          div(class = "ui form", id = "filters",
            div(class = "ui stackable two column grid container",
              div(class = "column", id = "col01and2",
                div(class = "row",
                  div(class = "ui noshadow horizontal segments",
                    tags$style(type='text/css', "#col01and2 { min-width: 550px; width: 550px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#col0 { min-width: 100px; width: 100px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#col1 { min-width: 250px; width: 250px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#col2 { min-width: 200px; width: 200px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#col3 { min-width: 300px; width: 300px;}"),
                    tags$style(type='text/css', "#col4 { min-width: 100px; width: 100px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#col5 { min-width: 200px; width: 200px; border-left: 0px;
          border-right: 0px;}"),
                    tags$style(type='text/css', "#steps { font-size: medium; }"),
                    div(class = "ui basic compact segment", id = "col0",
                      h2(class = "ui header",
                        uiicon("bullseye"),
                        div(class = "content", 1)
                      )
                    ),
                    div(class = "ui basic segment", id = "col1",
  p("Choose the selectivity levels for which you want chemical probes to be included in the library.", id = "steps")
                    ),
                    div(class = "ui basic segment", id = "col2",
  a(class = "ui red label", "Probes"),
  selectizeInput("probes", "", choices = list(`Best class` = "best", 
    `Second Class` = "second", `Non-specific` = "non", 
    `Unknown Selectivity` = "un"), selected = "best", multiple = T, options = list(
      'plugins' = list('remove_button')))
                    )
                  )
                ),
                div(class = "row",
                  div(class = "ui noshadow horizontal segments",
                    div(class = "ui basic compact segment", id = "col0",
                      h2(class = "ui header",
                        uiicon("pie chart"),
                        div(class = "content", 2)
                      )
                    ),
                    div(class = "ui basic segment", id = "col1",
  p("Select compounds in clinical development to be added to the library.", id = "steps")
                    ),
                    div(class = "ui basic segment", id = "col2",
  a(class = "ui red label", "Maximum clinical phase"),
  selectizeInput("clinical", "", choices = list(Approved = "approved", 
    `Phase III` = "three", `Phase II` = "two", `Phase I` = "one"), 
    selected = "approved", multiple = T, options = list(
      'plugins' = list('remove_button')))
                    )
                  )
                ),
                div(class = "row",
                  div(class = "ui noshadow horizontal segments",
                    div(class = "ui basic compact segment", id = "col0",
                        h2(class = "ui header",
                           uiicon("add square"),
                           div(class = "content", 3)
                        )
                    ),
                    div(class = "ui basic segment", id = "col1",
  p("Select compounds that are endorsed by other users to be added to the library", id = "steps")
                    ),
                    div(class = "ui basic segment", id = "col2",
  a(class = "ui red label", "Legacy compounds"),
  selectizeInput("legacy", "", choices = c(`Gray best inhibitor list` = "gray",
    `chemicalprobes.org 4.0 star rating` = "chem_probe"), multiple = T, options = list(
      'plugins' = list('remove_button')))
                    )
                  )
                )
              ),
              div(class = "centered column", id = "col3",
                div(class = "row",
                  div(class = "ui noshadow horizontal segments",
                    div(class = "ui basic segment", id = "col4",
                      h2(class = "ui header",
                        uiicon("options"),
                        div(class = "content", 4)
                      )
                    ),
                    div(class = "ui basic segment", id = "col5",
  p("Refine binding filters for compound in clinical development.", id = "steps")
                    )
                  )
                ),
                div(class = "ui center aligned basic compact segment", style="
    padding-top: 0px; margin: 0px;",
                  div(class = "row",
  sliderInput(inputId = "affinity", label = "Maximum Kd for query target (nM)",
              min = log10(10), max = log10(10000), value = log10(1000))
                  ),
                  div(class = "row",
  sliderInput(inputId = "meas", label = "Minimum number of measurements",
              min = 1, max = 40, value = 2)
                  ),
                  div(class = "row",
  sliderInput(inputId = "sd", label = "Maximum std. dev. of Kd (nM)",
              min = log10(10), max = log10(100000), value = log10(100))
                  )
                )
              )
            )
          ),
  h4(class="ui horizontal divider header", "Output table", uiicon("table"), style = "margin: 0px;"),
          div(class = "ui one column centered grid",
            div(class = "column",
  radioButtons(inputId = "table", "", choiceNames = c("Display per entry", "Display per compound"),
    choiceValues = c("entry", "cmpd"), inline = T),
  br(),
  DT::dataTableOutput("output_table")
            )
          )
        )
      )
      ),
      div(class = "ui bottom attached inverted footer segment", style = "margin: 0px;",
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