library(shiny)
library(shiny.semantic)
library(shinyjs)
library(markdown)

shinyUI(
  semanticPage(
    title = "HMS-LINCS Small Molecule Suite Applications",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    # Fix for mobile viewing
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
              box-sizing: border-box;
              }"),
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
        div(class = "ui red basic circular cancel icon button", 
            uiicon(type = "window close")
        )
      ),
      div(class = "ui center aligned basic segment",
        includeMarkdown("www/about.md")
      )
    ),
    div(class = "ui container",
      div(class = "ui top attached inverted five item stackable menu",
        div(class = "ui center aligned container",
  a(class = "item", img(class = "logo", src = "dcic.png"),
    href = "http://lincs-dcic.org/"),
  a(class = "item", "SelectivitySelectR", href = "/apps/SelectivitySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
  a(class = "item", "SimilaritySelectR", href = "/apps/SimilaritySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
  a(class = "item", "LibraryR", href = "/apps/LibraryR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
  a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
    href = "http://sorger.med.harvard.edu" )
        )
      ),
      div(class = "ui main center aligned attached segment", style = "font-size: medium;",
        includeMarkdown("www/home.md")
      ),
      div(class = "ui bottom attached inverted footer segment", style = "margin: 0px;",
        div(class = "ui center aligned container",
          div(class = "ui horizontal inverted large divided link list",
  a(class = "item", div(class = "action-button", "About", id = "about") ),
  a(class = "item", "Contact Us"),
  a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/smallmoleculesuite")
          )
        )
      )
    )
  )
)