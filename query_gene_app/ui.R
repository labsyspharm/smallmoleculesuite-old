library(shiny)
library(shiny.semantic)
library(shinyjs)
library(DT)

shinyUI(
  semanticPage(
    title = "Query Gene App",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
  box-sizing: border-box;
              }"),
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
      div(class = "ui top attached inverted five item menu",
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
      div(class = "ui main container attached segment",
        div(class="ui top secondary pointing menu", id = "tabs",
          a(class="item active", `data-tab`="tab1", "Gene input", id = "tab1_top"),
          hidden( a(class="item", `data-tab`="tab2", "Results", id = "tab2_top") )
        ),
        div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
          p("this is the main tab")
        ),
        div(class="ui bottom tab basic segment", `data-tab`="tab2", id = "tab2_bottom",
            style = "padding: 0px;",
          div(class = "ui basic segment", style = "padding: 0px;",
            p("this is the results tab")
          )
        )
      )
    ),
    div(class = "ui bottom attached inverted footer segment",
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