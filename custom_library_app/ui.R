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
    inlineCSS(".form-control {
  box-sizing: border-box;
              }"),
    tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 5px; background: black; border-top: 0px solid black; border-bottom: 0px solid black;}
               .irs-bar-edge {background: black; border: 0px solid black; height: 5px; width: 10px; border-radius: 0px;}
               .irs-line {border: 0px solid black; height: 5px; border-radius: 0px;}
               //.irs-grid-pol {display: none;}
               .irs-grid-text {font-size: 10px;}
               .irs-max {font-family: 'arial'; color: black;}
               .irs-min {font-family: 'arial'; color: black;}
               .irs-single {color:black; background:white; fond-size: 20px;}
               .irs-slider {width: 20px; height: 20px; top: 17px;}
               "),
    div(class = "ui mini modal",
      div(class = "actions",
        div(class = "ui red basic circular cancel icon button", uiicon(type = "window close")
        )
      ),
      div(class = "ui center aligned basic segment",
          h3("Website and application design"),
          p("Nienke Moret, Marc Hafner, Nicholas Clark"),
          h3("Application programming"),
          p("Nienke Moret (R scripts), Nicholas Clark (R/Shiny application)"),
          p("[others??]"),
          p("[Developed at Sorger lab, Harvard Medical School]"),
          br(),
          p("[cite paper here]"),
          h3("Links"),
          p("[sorger lab website]"),
          p("[github repo]"),
          p("LINCS/BD2K website?")
      )
    ),
    div(class = "ui container",
      div(class = "ui top attached inverted menu",
        div(class = "ui container",
          a(class = "header item", img(class = "logo", src = "logo_harvard_150.png") ),
          a(class = "item", "Query Drug App"),
          a(class = "item", "Query Gene App"),
          a(class = "item", "Custom Library App")
        )
      ),
      div(class = "ui main container attached segment",
        div(class="ui top secondary pointing menu", id = "tabs",
          a(class="item active", `data-tab`="tab1", "Gene input", id = "tab1_top"),
          hidden( a(class="item", `data-tab`="tab2", "Results", id = "tab2_top") )
        ),
        div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
          div(class = "ui stackable two column centered grid", 
            div(class = "five wide column",
              a(class = "ui red ribbon label", "Genes/targets"),
                div(class = "ui form",
  textAreaInput(inputId = "gene_list", label = "", value = "", placeholder = "MTOR\nRPS6KB1\nAKT\n...\netc.")
            ),
  br(),
  div(class = "ui button red action-button", "Submit", id = "submitButton"),
  br(),
  actionLink("load_example_kinases", "Load Example"),
  textOutput("gene_total"),
  textOutput("search_genes")
            ),
            div(class = "ten wide column",
              div(class = "ui form",
                div(class = "ui raised segment",
  h3("Description"),
  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi consectetur rutrum magna sed blandit. Aliquam erat volutpat. Aliquam erat volutpat. In hac habitasse platea dictumst. Suspendisse ut purus semper, dapibus odio nec, iaculis purus. Morbi ut scelerisque lacus. Cras tincidunt quis diam sit amet vestibulum.

Sed mollis faucibus turpis, a euismod sem condimentum ut. Sed vestibulum, neque nec gravida scelerisque, dui sapien iaculis est, at rutrum est nisl id lectus. Integer semper, lacus ac laoreet dapibus, velit nibh suscipit libero, vel imperdiet lacus nunc sed mauris. Donec condimentum lobortis porttitor. Ut nec leo eleifend sem ultricies dignissim at id mauris. Mauris a lacus aliquet, ultrices metus id, tempor arcu. Curabitur a mauris vel est lobortis mollis. Cras vehicula, metus ac rutrum porta, tellus quam tincidunt felis, eget maximus nulla ante molestie orci. Nunc sollicitudin lorem non aliquam ultrices. Integer lorem ante, sodales et efficitur ac, dictum tincidunt dui. Sed cursus nec ligula vitae iaculis. Integer venenatis tortor at justo dignissim, egestas aliquet dui ullamcorper. Aenean porta libero magna, sit amet ultricies mi sodales in. Donec non euismod dolor. Donec eget orci quis mi tincidunt sollicitudin.")
                )
              )
            )
          )
        ),
        div(class="ui bottom tab basic segment", `data-tab`="tab2", id = "tab2_bottom",
          div(class = "ui segment",
            div(class = "row",
    div(class = "circular ui icon button action-button", uiicon(type = "caret right", id = "filter_right"),
    hidden(uiicon(type = "caret down", id = "filter_down")), "Filters", id = "filter_button",
      uiicon(type = "filter")
    )
          ),
    hidden(
          br(),
          div(class = "ui form", id = "filters",
            div(class = "ui two column centered grid",
              div(class = "row",
                div(class = "four wide column",
  a(class = "ui red label", "Probes"),
  selectizeInput("probes", "", choices = list(`Best class` = "best", 
    `Second Class` = "second", `Non-specific` = "non", 
    `Unknown Selectivity` = "un"), selected = "best", multiple = T)
                ),
                div(class = "four wide column",
  sliderInput(inputId = "affinity", label = "Maximum Kd for query target (nM)",
    min = log10(10), max = log10(10000), value = log10(1000))
                )
              ),
              div(class = "row",
                div(class = "four wide column",
  a(class = "ui red label", "Maximum clinical phase"),
  selectizeInput("clinical", "", choices = list(Approved = "approved", 
    `Phase III` = "three", `Phase II` = "two", `Phase I` = "one"), 
    selected = "approved", multiple = T)
                ),
                div(class = "four wide column",
  sliderInput(inputId = "meas", label = "Minimum number of measurements",
    min = 1, max = 40, value = 2)
                )
              ),
              div(class = "row",
                div(class = "four wide column",
  a(class = "ui red label", "Legacy compounds"),
  selectizeInput("legacy", "", choices = c(`Gray best inhibitor list` = "gray", `chemicalprobes.org 4.0 star rating` = "chem_probe"), multiple = T)
                ),
                div(class = "four wide column",
  sliderInput(inputId = "sd", label = "Maximum std. dev. of Kd (nM)",
    min = log10(10), max = log10(100000), value = log10(100))
                )
              )
            )
          ))
  ),
          div(class = "ui one column centered grid",
            div(class = "column",
  radioButtons(inputId = "table", "", choiceNames = c("Display per entry", "Display per compound"),
    choiceValues = c("entry", "cmpd"), inline = T),
  br(),
  DT::dataTableOutput("output_table")
            )
          )
        )
      ),
      div(class = "ui inverted vertical footer segment",
        div(class = "ui center aligned container",
          div(class = "ui horizontal inverted small divided link list",
  a(class = "item", div(class = "action-button", "About", id = "about") ),
  a(class = "item", "Contact Us"),
  a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/drug_browser")
          )
        )
      )
    )
  )
)