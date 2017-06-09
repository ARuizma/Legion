
ui = fluidPage(
 sidebarPanel(fileInput('file1', 'Choose CSV File',
                        accept=c('text/csv', 
                                 'text/tab-separated-values, text/comma-separated-values,text/plain', 
                                 '.csv', '.txt', '.tsv')),
              selectInput("inSelect", "Select something", choices = "Pending Upload")
 ),
 mainPanel(
  tableOutput("contents"))
)