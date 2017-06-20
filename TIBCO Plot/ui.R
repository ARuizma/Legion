
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
        
   
   #tabPanel("Curve",
            # Plot output
    #         plotOutput('plot')
   #),
  
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
              fileInput('file1', 'Choose File with .csv format',
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
           ("Data Summary")
   )
   #tabPanel("Summary",
    #        withTags(					
     #        div(class = "col-sm-12",
      #           h3(id="model-summary", "Model(s) summary", align="center"),
       #          tableOutput('summary')
        #     )
         #   )
   #), 
   
  # tabPanel("Content",
   #         withTags(
    #         div(class = "col-sm-12",
     #            h3(id="content", "Data Content", align= "center"),
      #           tableOutput('content')
       #      )
        #    )
   #)
 )
   
 )
))