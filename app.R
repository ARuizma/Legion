require(shiny)
runApp(
 list(
  ui = fluidPage(
   sidebarPanel(fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                selectInput("inSelect", "Select something", choices = "Pending Upload")
   ),
   mainPanel(
    tableOutput("contents"))
  ),
  
  server = function(input, output, session) {
   contentsrea <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
     return(NULL)
    read.csv(inFile$datapath, check.names=FALSE)
   })
   output$contents<-renderTable({contentsrea()})
   observe({
    updateSelectInput(session, "inSelect", choices = names(contentsrea()))
    
   })
   print(typeof(contentsrea))
  }
 )
)