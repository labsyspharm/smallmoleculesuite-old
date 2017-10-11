library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)
library(readr)
library(ggvis)

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
    title = "Query Gene App",
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
    # CSS for hiding border on horizontal segments
    tags$style(type = "text/css", "
               .ui.noshadow.segments {
               box-shadow: none;
               border: none;
               border-left: 0px;
               margin-top: 0px;
               margin-bottom: 0px;
               padding: 0px;
               }"
    ),
    div(class = "ui mini modal",
      div(class = "actions",
        div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
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
              div(class = "stackable column", style = "width: 350px; min-width: 350px;",
    h3(class="ui horizontal divider header", uiicon("info circle"), "Instructions"),
    tags$style(type='text/css', "#instructions { font-size: medium; padding: 0px; margin: 0px;}"),
    tags$style(type='text/css', "#step1{ font-size: medium; padding: 0px; margin: 0px;}"),
    p(id = "instructions" ,"This app lets you see all compounds in the ", a("HMS Laboratory of Systems Pharmacology (LSP)", href = "http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology"), "collection that are shown to bind your gene target of interest."),
      br(), br(),
                div(class = "ui noshadow horizontal segments",
                  div(class = "ui basic compact segment", style = "width: 60px; min-width: 60px; padding: 0px;",
    h2(class = "ui header",
       img(class = "logo", src = "gene.png", style = "width: 31px; height: 31px; display: inline;"),
       div(class = "content", 1, style = "padding-left: 0px;")
    )
                  ),
                  div(class = "ui basic compact segment",
                    style = "border-left: 0px; padding: 0px;",
    p(id = "step1", "To find compounds, first select your target of interest (gene) from the box below.")
                  )
                ),
    br(),
    uiOutput("gene_search", style = "display: block; margin: auto; width: 200px;"),
    br(),
                div(class = "ui noshadow horizontal segments",
                  div(class = "ui basic compact segment",
                    style = "width: 60px; min-width: 60px; padding: 0px;",
    h2(class = "ui header",
      uiicon("options", style = "display: inline;", class = "red"),
      div(class = "content", 2, style = "display: inline; padding-left: 0px;")
    )
                  ),
                  div(class = "ui basic compact segment",
                    style = "padding: 0px; border-left: 0px;",
    p("Filter binding criteria for compound in clinical development.", style = "font-size: medium;")
                  )
                ),
    h3(class="ui horizontal divider header",
       div(class = "item action-button shiny-bound-input", id = "filter_button",
           a(class = "action-button", p(uiicon("caret down", id = "filter_right"),
                                        hidden(uiicon(type = "caret right", id = "filter_down")),
                                        "Show/hide filters", uiicon(type = "filter")), href = "#")
       )
    ),
                hidden(div(id = "filters",
    h5("Minimum/maximum affinity", style = "text-align: center; margin-top: 10px; margin-bottom: 10px;"),
    sliderInput("affinity", "", min = -3, max = 10, step = 1, value = c(-3,6)),
    h5("Maximum std. dev. of affinity", style = "text-align: center; margin-top: 10px; margin-bottom: 10px;"),
    sliderInput("sd", "", min = 0, max = 10, step = 1, value = 5),
    h5("Minimum number of measurements", style = "text-align: center; margin-top: 10px; margin-bottom: 10px;"),
    sliderInput("min_measurements", "", min = 1, max = 15, step = 1, value = 2)
                )
              )),
                div(class = "stackable column", style = "width: calc(100% - 350px); min-width: 400px;",
    h3(class="ui horizontal divider header", uiicon("bar chart"), "Main plot"),
                  div(class = "ui noshadow horizontal segments",
                      div(class = "ui basic compact segment",
                          style = "width: 60px; min-width: 60px; padding: 0px;",
                          h2(class = "ui header",
                             uiicon("crop", style = "display: inline;", class = "red"),
                             div(class = "content", 3, style = "display: inline; padding-left: 0px;")
                          )
                      ),
                      div(class = "ui basic compact segment",
                          style = "padding: 0px; border-left: 0px;",
                          p("Select a rectangle of the plot with your drug(s) of interest", style = "font-size: medium;")
                      )
                  ),
                  hidden(div(id = "plot_col",
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      hidden(div(class = "ui active text loader", id = "loader1", "Loading Plot"))
    ),
    ggvisOutput("mainplot")
                  )
                )
              )
            ),
            hidden(div(class = "row", id = "table_row",
              div(class = "column", style = "min-height: 200px;",
    h3(class="ui horizontal divider header", uiicon("table"), "Output table"),
    h4(class = "ui centered header", "Select rows below to see drug targets and binding affinities for a given drug."),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
      hidden(div(class = "ui active text loader", id = "loader_table", "Loading Table"))
    ),
    DT::dataTableOutput("output_table"),
    tags$style(type='text/css', "#output_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
              )
            )),
    tags$style(type = "text/css", "#row3_col1 { width: calc(100%/3); min-width: 350px; }"),
    tags$style(type = "text/css", "#row3_col2 { width: calc(100%/3); min-width: 350px; }"),
    tags$style(type = "text/css", "#row3_col3 { width: calc(100%/3); min-width: 350px; }"),
            hidden(div(class = "row", id = "result_row3",
    tags$style(type='text/css', "#sel_drug1 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
    tags$style(type='text/css', "#sel_drug2 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
    tags$style(type='text/css', "#sel_drug3 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
               hidden(div(class = "padded stackable column", id = "row3_col1",
                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 1"), 
                          h4(class = "ui centered header", textOutput("sel1_drug")),
                          DT::dataTableOutput("selection1", width = "250px")
               )),
               hidden(div(class = "stackable column", id = "row3_col2",
                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 2"),
                          h4(class = "ui centered header", textOutput("sel2_drug")),
                          DT::dataTableOutput("selection2", width = "250px")
               )),
               hidden(div(class = "stackable column", id = "row3_col3",
                          h3(class="ui horizontal divider header", uiicon("table"), "Selection 3"),
                          h4(class = "ui centered header", textOutput("sel3_drug")),
                          DT::dataTableOutput("selection3", width = "250px")
               ))
    )),
    hidden(div(class = "row", id = "button_row",
      div(class = "ui secondary button action-button", "Clear selections", id = "clearButton")
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