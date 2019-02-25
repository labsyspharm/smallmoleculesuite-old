library(shiny)
library(shinyjs)

about.modal.js = "$('.ui.small.modal')
$('#about_modal').modal('show')
;"
contact.modal.js = "$('.ui.mini.modal')
$('#contact_modal').modal('show')
;"

shinyServer(function(input, output, session) {
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  observeEvent(input$contact, {
    runjs(contact.modal.js)
  })
  session$allowReconnect(TRUE)
})