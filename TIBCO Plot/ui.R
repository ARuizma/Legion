library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

shinyUI <- tagList(
 
 dashboardPage(skin = "green", 
 
#HEADERPANEL####
  
  dashboardHeader(title = "Legion"),
   #h2("We are Legion, for we are many")
  
#SIDEBAR####
  
  
  dashboardSidebar(
   
 
  sidebarMenu(
   
   menuItem("Data Upload", tabName="daup", icon = icon("file-o")),

   menuItem("CurveFitting", tabName = "curvefitting", icon = icon("line-chart")),
   
   menuItem("Dim Reduction", tabName = "dimred", icon = icon("resize-small", lib = "glyphicon")),
   
   menuItem("Clustering", tabName = "clustering", icon = icon("spinner"))
   
)),

#MAINPANEL####
 
 dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  tabItems(
   tabItem(tabName = "daup",
           
           fluidRow(
            column(width = 2,
                          
                          box(title = "File Information", side = "right",
                                 id = "tabset1", width = NULL,
                                 
                                 #FILETAB#
                                  
                                  
                                   fileInput('file1', 'Choose File with .csv, .tsv, .txt format',
                                             accept=c('text/csv', 
                                                      'text/comma-separated-values, text/plain', 
                                                      '.csv')
                                   
                                  ),
                                  
                                  
                                   checkboxInput('header', 'Headers', TRUE),
                                
                                  
                                  
                                   radioButtons('sep', "Separation",
                                                c(Comma=',',
                                                  Semicolon=';',
                                                  Tab='\t'),
                                                ',')
                                  
                                 )
                          ),
           
                   #CONTENT#
            column(width = 10, 
                   box(
                    title = "Data Content",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    DT::dataTableOutput("content"),width = NULL)
            #box(
             #title = "Data Histogram",
             #collapsible = TRUE,
             #collapsed = FALSE,
             #plotlyOutput("hist"), width = NULL)
           ))),
   
#CURVEFITTING####
   
  tabItem(tabName = "curvefitting", 
          fluidRow(
           column(width = 2,
                  #SETTINGSTAB#
                  box(title = "Settings",
                          width = NULL,
                            checkboxInput("nplr_checkbox", label = "NPLR", value = FALSE),
                            uiOutput('npars'),
                            checkboxInput("nls_checkbox", label = "NLS", value = FALSE),
                            selectInput('zcol', 'Compounds', choices = "Pending Upload"),
                            selectInput('xcol', 'Concentration', choices = "Pending Upload"),
                            selectInput('ycol', 'Feature', choices = "Pending Upload"),
                            downloadButton('down', 'Download Summary', class = 'text-center', style = 'width:100%;'))),
 column(width = 10, 
 box(title = "Visualization", width = NULL,
  #PLOT#
   box(
   title = "Plot",
   collapsible = TRUE,
   collapsed = FALSE,
   plotlyOutput("plot"), width = NULL),
  #SUMMARY#
  box(
   title = "Summary",
   collapsible = TRUE,
   collapsed = FALSE,
   DT::dataTableOutput("summary"), width = NULL))))),

#DIMENSIONALITYREDUCTION####

tabItem(tabName = "dimred", 
        
        fluidRow(
         column(width = 2, 
                box(
                 title = "Settings", width = NULL,
                 selectInput('dcol', 'Select Data', choices = "Pending Upload", selectize = TRUE, multiple = TRUE),
                 selectInput('colcol', 'Color By', choices = "Pending Upload"),
                 radioButtons('dim', 'Dimension Visualization', choices = list("2D" = 2, "3D" = 3), selected = 2),
                 uiOutput('grad'),
                 checkboxInput("tsne_checkbox", label = "TSNE", value = FALSE),
                 uiOutput('perp'),
                 checkboxInput("pca_checkbox", label = "PCA", value = FALSE)
                )),
         box(
          title = "TSNE",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("tsne"), width = 5),
         
         box(
          title = "PCA",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("pca"), width = 5)

)),

#CLUSTERING####
tabItem(tabName = "clustering", 
        fluidRow(
         column(width = 2, 
                box(title = "Settings", width = NULL,
                 selectInput('ccol', 'Select Data', choices = "Pending Upload", selectize = TRUE, multiple = TRUE),
                 selectInput('axcol', 'X axis', choices = "Pending Upload"),
                 selectInput('aycol', 'Y axis', choices = "Pending Upload"),
                 sliderInput('num', 'Clusters', min = 2, max = 10, value = 3),
                 checkboxInput("kmeans_checkbox", label = "K-means", value = FALSE),
                 uiOutput('alg'),
                 checkboxInput("hieclu_checkbox", label = "Hierarchical Clustering", value = FALSE),
                 uiOutput('met'),
                 downloadButton('down2', 'Download Summary', style = 'width:100%;'))),
                 
         box(
          title = "K-Means",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("kmeans"), width = 5),
         
         box(
          title = "Hierarchical Clustering",
          collapsible = TRUE,
          collapsed = FALSE,
          plotlyOutput("hieclu"), width = 5),

         box(
          title = "Summary",
          collapsible = TRUE,
          collapsed = FALSE,
          DT::dataTableOutput("sumclus"),width = 10)))))),
#FOOTER####
 
tags$footer(align = "center", style = ("background-color: transparent; color: black;
                                       bottom:0; width:100%; 
                                       padding: 10px;"),
withTags(
 div(class="row",
     div(class="col-sm-12 footer",
         span(class="calligraffitti", "Designed By"),
         span(id="github",
              a("Andrés Ruiz Marín",
                href="https://github.com/Aruma27/Bioenterprise-Perkin-Elmer",
                target="_top")),
         span(class="calligraffitti", "In Colaboration with"),
         span(class="lato", "Oscar Mora"),
         span(HTML("&copy;2017. "))))))
)#END