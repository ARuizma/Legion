library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

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
   
   menuItem("Dim Reduction", tabName = "dimred", icon = icon("th")),
   
   menuItem("PHB", tabName = "phb", icon = icon("dashboard"))
   
)),

 #MainPanel####
 
 dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
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
            column(width = 9, 
                   box(title = "Data", width = NULL,
                       tabPanel("",

                   box(
                    title = "Data Content",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    DT::dataTableOutput("content"),width = NULL),
            box(
             title = "Data Histogram",
             collapsible = TRUE,
             collapsed = FALSE,
<<<<<<< HEAD
             plotlyOutput("hist"), width = NULL)
           ))))),
=======
             plotlyOutput("hist", height = 500), width = 9, height = 500)
           )),
>>>>>>> DimRed
   
 #CURVEFITTING####
   
  tabItem(tabName = "curvefitting", 

          fluidRow(
           column(width = 3,
                  
                  #SETTINGSTAB####
                  box(title = "Settings",
                          width = NULL,
                         tabPanel(" ",
                           
                           box(
                            title = "Select Model", width = NULL,
                            checkboxInput("nplr_checkbox", label = "NPLR", value = FALSE),
                            checkboxInput("nls_checkbox", label = "NLS", value = FALSE)
                           ),

                           box(
                            title = "Choose Number of Parameters", br(), "(NPLR only)", width = NULL,
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
                           )))),

 column(width = 9, 
 box(title = "Visualization", width = NULL,
        tabPanel("",
  
 #NPLR####
  
  
  #PLOT####
         
   box(
   title = "Plot",
   collapsible = TRUE,
   collapsed = FALSE,
   plotlyOutput("plot"), width = NULL),
  
  #SUMMARY####
  
  box(
   title = "Summary",
   collapsible = TRUE,
   collapsed = FALSE,
   DT::dataTableOutput("summary"), width = NULL)
       
  )
<<<<<<< HEAD
)))
=======
))
),

#DIMENSIONALITYREDUCTION####

tabItem(tabName = "dimred", 
        
        fluidRow(
         column(width = 3, 
                box(
                 title = "SelectData", width=NULL,
                 selectInput('ccol', '', choices = "Pending Upload", selectize = TRUE, multiple = TRUE)
                 )),
         box(
          title = "TSNE",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("drtsne"), width = NULL),
         
         box(
          title = "PCA",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("drpca"), width = NULL)
>>>>>>> DimRed
))
))
))#END