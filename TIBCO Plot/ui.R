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
   

   menuItem("CurveFitting", tabName = "curvefitting", icon = icon("dashboard"),
            
            menuSubItem("NPLR", tabName = "nplr", icon = icon("dashboard")),
            
            menuSubItem("PKI", tabName = "pki", icon = icon("dashboard")),
            
            menuSubItem("Custom", tabName = "custom", icon = icon("dashboard"))),
   
   menuItem("TSNE", tabName = "tsne", icon = icon("th"),
            
            menuSubItem("Custom", tabName = "customts", icon = icon("dashboard"))),
  
   menuItem("PHB", tabName = "phb", icon = icon("dashboard"))
   
)),

 #MainPanel####
 
 dashboardBody(
  tabItems(
   
  
  tabItem(tabName = "nplr", 

          fluidRow(
           
           column(width = 3,
                  
                  tabBox(title = "Data Customization",
                         id = "tabset1", width = NULL,
 
  #FILETAB####
                  
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
                   
                   
                  ),

  #SETTINGSTAB####        
           
                   tabPanel("Settings",
        
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
                    selectInput('zcol', '', choices = "Pending Upload"
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
                   ))
                  )),
  
  #CONTENT####
  box(
   title = "Data Content",
   collapsible = TRUE,
   collapsed = TRUE,
   DT::dataTableOutput("content"),width = 8),
  
  #PLOT#####
  
  box(plotOutput("nplrplot",height = 500), width = 8, height = 500),
  
  #SUMMARY####
  
  box(
   title = "Summary",
   DT::dataTableOutput("nplrsummary"), width = 8)
       
  )
  
  ),
  
  tabItem(tabName = "pki",
          
          fluidRow(
           
           column(width = 5,
                  
                  tabBox(title = "Data Customization",
                         id = "tabset1", width = NULL,
                         
                         #FILETAB####
                         
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
                         ),
                         
                         #SETTINGSTAB####        
                         
                         tabPanel("Settings",
                                  
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
                                   selectInput('zcol', '', choices = "Pending Upload"
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
                                  ))
                  ))
           
           
          )
          
          
  
  )
       
))
))