library(shiny)
library(shinyjs)

about.modal.js = "$('.ui.mini.modal')
.modal('show')
;"

shinyServer(function(input, output, session) {
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })

})