library(shiny)
library(shinydashboard)

shinyUI(
 
 dashboardPage( 
 
 #HeaderPanel####
  
  dashboardHeader(title = "Legion"),
   #h2("We are Legion, for we are many")
  
 #SIDEBAR####
  
  
  dashboardSidebar(
  
  sidebarMenu(
   
   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),

   menuItem("Content", tabName = "content", icon = icon("th")),
      
   menuItem("Summary", tabName = "summary", icon = icon("th")),
   
   menuItem("Tests", tabName = "test", icon = icon("th")),

   tags$br()
)
),

 #MainPanel####
 
 dashboardBody(
  tabItems(
  
  tabItem("dashboard",
          fluidRow(
          box(plotOutput("plot", height = 250)),
          
          box(
           title = "Customize Data Output",
           
           box(
            title = "Log",
            selectInput("plot_scaletype", "Scale type",
                        c("normal" = "normal",
                          "log" = "log"
                        ),
                        selected = "log",
                        selectize = FALSE
            )
           ),
           
           #box(
            #title = "",
            #checkboxInput("props", "Convert to Prop", FALSE)
           #),
           
           box(
            title = "Choose Number of Parameters",
            selectInput('npars', '', c("Best" = 'all',
                                       "2" = '2',
                                       "3" = '3',
                                       "4"= '4',
                                       "5"='5'), "all"
                        )
           ),
            
           box(
            title = "Choose Data Names",
            selectInput('zcol', '', choices = "Pending Upload"
            )
           ),
           
           box(
            title = "Choose X axis",
            selectInput('xcol', '', choices = "Pending Upload"
            )
           ),
           
           box(
            title = "Choose Y axis",
            selectInput('ycol', '', choices = "Pending Upload"
            )
           )
          )
          )
  ),
  
  tabItem("content",
          fluidRow(
           
           box(
            title = "Uploaded Data",
            tableOutput('content')
           ),

           box(
             title = "Customize Data Input",
            
             box(
              title = "Upload File",
              fileInput('file1', 'Choose File with .csv, .txt or .tsv format',
                        accept=c('text/csv', 
                                 'text/comma-separated-values, text/plain', 
                                '.csv')
              )
             ),
             
             box(
              title = "",
              checkboxInput('header', 'Headers', TRUE)
             ),
                      
             box(
              title = "Separation Type",
              radioButtons('sep', "",
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           ',')
             )
            )
           )
   ),
             
   tabItem("summary",
           title = "Data Summary",
           tableOutput('summary')
   ),
   
  tabItem("test",
          title = "Tests",
          tableOutput('test')
  )
   
 )
   
 )
))