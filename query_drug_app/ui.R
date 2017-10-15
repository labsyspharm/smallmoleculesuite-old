library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)
library(plotly)
library(readr)

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
return (Math.pow(2, num).toLocaleString());
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
//logifySlider('sd')
//logifySlider('affinity')
logifySlider('n_pheno')
logifySlider('n_common')
}, 5)})
"

shinyUI(
  semanticPage(
    title = "Query Drug App",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "drug_app.css")
    ),
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
        includeMarkdown("www/about.md")
      )
    ),
    div(class = "ui container", style = "width: 1125px; min-width: 1125x; margin: 0px;",
      div(class = "ui top attached inverted five item stackable menu", style = "width: 100%;",
        div(class = "ui center aligned container",
            a(class = "item", img(class = "logo", src = "dcic.png"),
              href = "http://lincs-dcic.org"),
            a(class = "item", "Custom Library App", href = "http://shiny.ilincs.org/custom_library_app/"),
            a(class = "active item", "Query Drug App", href = "http://shiny.ilincs.org/query_drug_app/"),
            a(class = "item", "Query Gene App", href = "http://shiny.ilincs.org/query_gene_app/"),
            a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
              href = "http://sorger.med.harvard.edu" )
        )
      ),
      div(class = "ui main container attached segment", style = "margin: 0px; padding-right: 0px; padding-left: 0px;",
        div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
          div(class = "ui grid",
            div(class = "row",
              div(class = "stackable column", style = "width: 300px; min-width: 300px;",
                div(class = "ui basic center aligned segment",
  h4(class="ui header", "Select reference compound"),
  selectizeInput(inputId = "query_compound", label = "", choices = sort(unique(similarity_table$name_1)),
                 options = list(
                   placeholder = 'Search for a compound',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )
  #uiOutput("drug_search")
                )
              ),
              div(class = "stackable column", style = "width: calc(100% - 300px); min-width: calc(100% - 300px);",
                div(class = "ui basic segment", style = "font-size: medium;",
  h3(class="ui horizontal divider header",
    div(class = "item action-button shiny-bound-input", id = "intro_hide",
      a(class = "action-button", p(uiicon("caret down", id = "caret_down"),
      hidden(uiicon(type = "caret right", id = "caret_right")),
      "Instructions", uiicon(type = "info_circle")), href = "#")
     )
  ),
  div(id = "intro",
    includeMarkdown("www/intro.md")
  )
                )
              ),
  tags$style(type='text/css', "#steps { font-size: medium; }"),
              div(class = "stackable column", style = "width: 350px",
  tags$style(type='text/css', "#col0 { min-width: 100px; width: 100px; border-left: 0px;
          border-right: 0px;}"),
  tags$style(type='text/css', "#col1 { border-left: 0px; border-right: 0px;}")
              )
            ),
            hidden(div(class = "row", id = "result_row1", style = "margin: 0px; padding: 0px",
              div(class = "ui basic center aligned segment",
  h3(class="ui horizontal divider header", uiicon("bar chart"), "Compound similarity plots"),
  h5("Select an area of similarity you are interested in. ", intToUtf8(160),intToUtf8(160), " Hover over points for more information. ", intToUtf8(160),intToUtf8(160), " Double-click on plot to un-select region.",
     style = "margin: 0px; padding: 0px"),
  h3(class = "ui centered header", textOutput("ref_drug"))
            ))),
            hidden(div(class = "row", style = "height: 450px", id = "result_row2",
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader1",
                            "Loading Plot 1"))),
  plotlyOutput("mainplot1")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader2",
                            "Loading Plot 2"))),
  plotlyOutput("mainplot2")
              ),
              div(class = "stackable five wide column",
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                 hidden(div(class = "ui active text loader", id = "loader3",
                            "Loading Plot 3"))),
  plotlyOutput("mainplot3")
              )
            )),
            hidden(div(class = "row", style = "margin: 0px; padding: 0px;", id = "filters_head",
              div(class = "ui basic center aligned segment",
  h3(class="ui horizontal divider header",
    div(class = "item action-button shiny-bound-input", id = "filter_button",
      a(class = "action-button", p(uiicon("caret down", id = "caret_down_fil"),
      hidden(uiicon(type = "caret right", id = "caret_right_fil")),
      "Set similarity thresholds", uiicon(type = "filter")), href = "#")
     )
  )
              )
            )),
            hidden(div(class = "row", style = "margin: 0px; padding: 0px;",
              id = "filters",
              div(class = "four wide column"),
              div(class = "four wide column",
sliderInput("n_common", "Number of biological assays in common with reference compound
            ", min = 0, max = 8, step = 1, value = 0)
),
              div(class = "four wide column",
sliderInput("n_pheno", "Number of phenotypic assays in common with reference compound
            ", min = 0, max = 8, step = 1, value = 0)
)
                )),
  tags$style(type = "text/css", "#row3_col1 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_col2 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_col3 { width: 375px; min-width: 375px; }"),
  tags$style(type = "text/css", "#row3_col4 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_col5 { width: calc((100% - 375px)/2); }"),
  tags$style(type = "text/css", "#row3_bind_data { width: 375px; min-width: 375px; }"),
            hidden(div(class = "row", id = "result_row4",
              div(class = "column", style = "min-height: 200px;",
  h3(class="ui horizontal divider header", uiicon("table"), "Output table"),
  h4(class = "ui centered header", "Select up to three similar compounds for which target affinity information will be displayed"),
              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                hidden(div(class = "ui active text loader", id = "loader_tab", "Loading Table",style = "margin-top: 50px; margin-bottom: 50px;"))),
   DT::dataTableOutput('data_table'),
   tags$style(type='text/css', "#data_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
             )
            )),
            hidden(div(class = "row", id = "result_row3",
             div(class = "stackable center aligned column", id = "row3_bind_data", style = "font-size: medium;",
   h3(class="ui horizontal divider header", uiicon("table"), "Binding data"),
   tags$style(type='text/css', "#binding_data { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug1 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug2 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   tags$style(type='text/css', "#sel_drug3 { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;"),
   textOutput("binding_drug", inline = T),
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
              ))
              )),
  div(class = "row", id = "result_row_next",
              hidden(div(class = "stackable column", id = "row3_col3",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 3"),
  h4(class = "ui centered header", textOutput("sel3_drug")),
  DT::dataTableOutput("selection3")
              )),
              hidden(div(class = "stackable column",  id = "row3_col4",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 4"),
  h4(class = "ui centered header", textOutput("sel4_drug")),
  DT::dataTableOutput("selection4")
              )),
              hidden(div(class = "stackable column", id = "row3_col5",
  h3(class="ui horizontal divider header", uiicon("table"), "Selection 5"),
  h4(class = "ui centered header", textOutput("sel5_drug")),
  DT::dataTableOutput("selection5")
              ))
            ),
            hidden(div(class = "row", id = "button_row",
  div(class = "ui secondary button",
      downloadLink("downloadBind", "Download binding data (.csv)", style = "color: white;")
      ),
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