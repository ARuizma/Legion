library(shiny)
library(shinydashboard)
library(DT)

shinyUI(
 
 dashboardPage(skin = "green", 
 
 #HeaderPanel####
  
  dashboardHeader(title = "Legion"),
   #h2("We are Legion, for we are many")
  
 #SIDEBAR####
  
  
  dashboardSidebar(
   
 
  sidebarMenu(
   
   menuItem("Data Upload", tabName="daup", icon = icon("dashboard")),

   menuItem("CurveFitting", tabName = "curvefitting", icon = icon("dashboard")),
   
   menuItem("TSNE", tabName = "tsne", icon = icon("th"),
            
            menuSubItem("Custom", tabName = "customts", icon = icon("dashboard"))),
  
   menuItem("PHB", tabName = "phb", icon = icon("dashboard"))
   
)),

 #MainPanel####
 
 dashboardBody(
  tabItems(
   tabItem(tabName = "daup",
           
           fluidRow(
            column(width = 3,
                          
                          tabBox(title = "File Information", side = "right",
                                 id = "tabset1", width = NULL,
                                 
                                 #FILETAB####
                                  tabPanel(" ",
                                  box(
                                   title = "Upload File", width = NULL,
                                   fileInput('file1', 'Choose File with .csv, .tsv, .txt format',
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
           
                   #CONTENT####
           
                   box(
                    title = "Data Content",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    DT::dataTableOutput("content", height = 750),width = 9, height = 750)
           )),
   
 #######################################################CURVEFITTING##########################################
   
  tabItem(tabName = "curvefitting", 

          fluidRow(
           column(width = 3,
                  
                  #SETTINGSTAB####
                  tabBox(title = "Settings",
                          width = NULL,
                         tabPanel(" ",
                           
                           box(
                            title = "Choose Number of Parameters", width = NULL,
                            selectInput('npars', '', c("Best" = 'all',
                                                       "2" = '2',
                                                       "3" = '3',
                                                       "4"= '4',
                                                       "5"='5'), "all"
                            )
                           ),
                           
                           box(
                            title = "Choose Data Names", width = NULL,
                            selectInput('zcol', 'Names', choices = "Pending Upload"
                            )
                           ),
                           
                           box(
                            title = "Choose X axis", width = NULL,
                            
                            selectInput('xcol', '', choices = "Pending Upload"
                            )
                           ),
                           
                           box(
                            title = "Choose Y axis", width = NULL,
                            selectInput('ycol', '', choices = "Pending Upload"
                            )
                           )))),
           column(width = 9,
  
 tabBox(title = "Visualization", width = NULL,
  
 #######################################################NPLR##########################################
  
 tabPanel("NPLR",
  
  #PLOT#####
         
   box(
   title = "Plot NPLR",
   collapsible = TRUE,
   collapsed = FALSE,
   plotOutput("nplrplot",height = 500), width = NULL),
  
  #SUMMARY####
  
  box(
   title = "Summary NPLR",
   collapsible = TRUE,
   collapsed = FALSE,
   DT::dataTableOutput("nplrsummary"), width = NULL
       
  )),
  
 ###########################################NLS####################################### 
  
 tabPanel("NLS",
         
          #PLOT#####
          
          box(
           title = "Plot NLS",
           collapsible = TRUE,
           collapsed = FALSE,
           plotOutput("nlsplot",height = 500), width = NULL),
          
          #SUMMARY####
          
          box(
           title = "Summary NLS",
           collapsible = TRUE,
           collapsed = FALSE,
           DT::dataTableOutput("nlssummary"), width = NULL
          
         )
         
 )
       
)))
))
))
)