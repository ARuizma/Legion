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
   

   menuItem("CurveFitting", tabName = "curvefitting", icon = icon("dashboard"),
            
            menuSubItem("NPLR", tabName = "nplr", icon = icon("dashboard")),
            
            menuSubItem("PKI", tabName = "pki", icon = icon("dashboard")),
            
            menuSubItem("Custom", tabName = "custom", icon = icon("dashboard"))),

   menuItem("TSNE", tabName = "tsne", icon = icon("th"),
            
            menuSubItem("Custom", tabName = "customts", icon = icon("dashboard"))),
      
   menuItem("PHB", tabName = "phb", icon = icon("dashboard")),
   
   menuItem("Summary", tabName = "summary", icon = icon("dashboard"))

              #tags$br()
   
),


             
   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),

   menuItem("Content", tabName = "content", icon = icon("th")),
      
   menuItem("Summary", tabName = "summary", icon = icon("th")),
   
   menuItem("Tests", tabName = "test", icon = icon("th")),

   tags$br()

),

 #MainPanel####
 
 dashboardBody(
  tabItems(
  
  tabItem("nplr",
          fluidRow(
           
           column(width = 3,
        
          tabBox(title = "Data Customization", side = "right",
                 id = "tabset1", width = NULL,
           
           tabPanel("Settings",
            
            box(
            title = tags$p("Log", width = NULL, style = "font-size: 150%;"),
  tabItem("dashboard",
          fluidRow(
          box(plotOutput("plot", height =500)),
          
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
            title = "Choose Data Names", width = NULL,
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
            title = "Choose X axis", width = NULL,
            title = "Choose X axis",
            selectInput('xcol', '', choices = "Pending Upload"
            )
           ),
           
           box(
            title = "Choose Y axis", width = NULL,
            selectInput('ycol', '', choices = "Pending Upload"
            )
           )
          ),
          
          tabPanel(
           title = "File Information", width = NULL,
           
           box(
            title = "Upload File", width = NULL,
            fileInput('file1', 'Choose File with .csv format',
                      accept=c('text/csv', 
                               'text/comma-separated-values, text/plain', 
                               '.csv')
            )
           ),
           
           box(
            title = "", width = NULL,
            checkboxInput('header', 'Headers', TRUE)
           ),
           
           box(
            title = "Separation Type", width = NULL,
            radioButtons('sep', "",
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ',')
           )
          )
          )),
       box(plotOutput("plot", height = 500, width = 50))
  )),
        
             
   tabItem("summary",
           ("Data Summary")
   ),
  
  #box(
   #title = "Uploaded Data",
   #tableOutput('content')
 # ),
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
)
))
