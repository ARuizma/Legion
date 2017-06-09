
shinyUI(
 
 fluidPage(
  
 #HeaderPanel####
  
  headerPanel(
   strong("Legion")
   #h2("We are Legion, for we are many")
  ),
  
 #SIDEBAR####
  
  
 sidebarPanel(fileInput('file1', 'Choose File with .csv format',
                        accept=c('text/csv', 
                                 'text/comma-separated-values, text/plain', 
                                 '.csv')),
              tags$br(),
              checkboxInput('header', 'Header', TRUE),
              radioButtons('sep', 'Separator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           ','),
              radioButtons('quote', 'Quote',
                           c(None='',
                             'Double Quote'='"',
                             'Single Quote'="'"),
                           '"'),
              selectInput("plot_scaletype", "Scale type",
                          c("normal" = "normal",
                            "log" = "log"
                          ),
                          selected = "log",
                          selectize = FALSE
              ),
              
              selectInput('xcol', 'Select X variable', choices = "Pending Upload"),

 
              selectInput('ycol', 'Select Y variable', choices = "Pending Upload")
 ),


             
 #MainPanel####
 
 mainPanel(
  tabsetPanel(
   
   tabPanel("Curve",
            # Plot output
             plotOutput('plot')
   ),
  
   tabPanel("Summary",
            withTags(					
             div(class = "col-sm-12",
                 h3(id="model-summary", "Model(s) summary", align="center"),
                 tableOutput('summary')
             )
            )
   ), 
   
   tabPanel("Content",
            withTags(
             div(class = "col-sm-12",
                 h3(id="content", "Data Content", align= "center"),
                 tableOutput('content')
             )
            )
   )
 )
   
 )
)
)

