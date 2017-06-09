library(shiny)


server = function(input, output, session) {
 upfile <- reactive({
  file1 <- input$file1
  if (is.null(file1))
   return(NULL)
  read.csv(file1$datapath, check.names=FALSE)
 })
 output$contents<-renderTable({upfile()})
 observe({
  updateSelectInput(session, "inSelect", choices = names(upfile()))
  
 })
}
