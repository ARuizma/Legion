 
shinyUI(
 fluidPage(
  
  ########################################
  # TITLE
  
  headerPanel(
   
   strong("Legion")
   #h6("We are Legion, for we are many")
  ),
  
  ########################################
  # SIDEBAR PANEL
  sidebarPanel(
   
   
   ########################################
   # CHOOSE FILE
   
   withTags(
    div(
     class="row",
     div(class="col-xs-12 btn-input",
         div(class="col-xs-6 checkboxText", "File with headers"),
         div(class="col-xs-6", checkboxInput('header', 'yes', TRUE))
     ),
     div(class="col-xs-12", id="inputFile",
         fileInput('file1', '',
                   accept=c('text/csv',
                            'text/tab-separated-values,text/comma-separated-values,text/plain',
                            '.csv', '.txt', '.tsv'
                   )
         )
     )
    )
   ),
   
   
   ########################################
   # TRANSFORM
   withTags(div(class="col-sm-12 section-title", h3("Tranform"))),
   withTags(
    div(class="row",
        div(class="col-xs-12 btn-input",
            div(class="col-xs-6 checkboxText", "Convert Log10[conc.]"),
            div(class="col-xs-6", checkboxInput('toLog', 'yes', TRUE))
        )
    )
   ),
   withTags(
    div(class="row",
        div(class="col-xs-12 btn-input",
            div(class="col-xs-6 checkboxText", "Compute props"),
            div(class="col-xs-6", checkboxInput('props', 'yes', FALSE))
        )
    )
   ),
   
   ########################################
   # ANALYSE
   withTags(div(class="col-sm-12 section-title", h3("Analyse"))),
   withTags(
    div(class="col-sm-12 btn-input",
        h4("Number of parameters"),
        radioButtons('npar', '', c("best"='all', "2"='2', "3"='3', "4"='4', "5"='5'), "all")
    )
   ),
   
   ########################################
   # Settings
   withTags(div(class="col-sm-12 section-title", h3("Visualize"))),
   
   # HIGHLIGHT
   uiOutput('modelNames'),
   uiOutput('selectBox'),
   
<<<<<<< HEAD
   #SELECTBOX####
   withTags(
    div(class='col-sm-12',
        div(class = "row",
            div(class="col-xs-12 radioText", "Select Data"),
            div(class="col-xs-12 btn-input",
                div(class="col-xs-4", selectInput(inputId="selectData", label = " ", choices = "emtpy..."))
                )
            )
    )
   ),
   
=======
   #withTags(
    #div(class = 'col-sm-12',
     #   div(class = "row",
      #      div(class="col-xs-12 radioText", "Select data"),
       #     div(class="col-xs-12", selectInput("inselect", "Select Data", choices = "Pending upload")
        #    )
        #)
    #)
   #),
>>>>>>> 424ddf657da18176d039623de76ff31814b5ff64
  
   # SAVE OPTIONS
   withTags(
    div(class='col-sm-12',
        
        div(class = "row",
            div(class="col-xs-12 radioText", "Show values"),
            div(class="col-xs-12 btn-input",
                div(class="col-xs-4", checkboxInput(inputId = "points", label = "Points", value=FALSE)),
                div(class="col-xs-4", checkboxInput(inputId = "Means", label = "Means", value=TRUE)),
                div(class="col-xs-4", checkboxInput(inputId = "SDerr", label = "SDerr", value=TRUE))
            )
        ),
        
        # Show x-axis as Log
        div(class="row",
            div(class="col-xs-12 btn-setting",
                div(class="col-xs-6 checkboxText", "Conc. in Log10"),
                div(class="col-xs-6", checkboxInput('showAsLog', 'yes', TRUE))
            )
        ),
        
        # Show legend
        div(class="row",
            div(class="col-xs-12 btn-setting",
                div(class="col-xs-6 checkboxText", "Show legend"),
                div(class="col-xs-6", checkboxInput('showLegend', 'yes', TRUE))
            )
        ),
        
        # Name axes
        div(class="row",
            div(class='col-xs-6', textInput("xlabel", 'x-axis name', "Log10(Conc.)")),
            div(class='col-xs-6', textInput("ylabel", 'y-axis name', 'Response (Vs. control)'))
        )
    )
   )
  ),
  # end sidebarPanel
  
  ########################################
  # OUTPUTS PANEL
  mainPanel(
   tabsetPanel(
    
    tabPanel("Curve",
             
             # Plot setting Sliders
             withTags(
              div(class="row sliders-panel",
                  
                  # Points size slider
                  div(class="col-xs-4",
                      div(class="col-xs-12 radioText", "Points size"),
                      div(class="col-xs-12 slider-input",
                          sliderInput("pSize", label="", min=0, max=1, value=.3, step=.01))
                  ),
                  
                  # Line width slider
                  div(class="col-xs-4",
                      div(class="col-xs-12 radioText", "Lines width"),
                      div(class="col-xs-12 slider-input",
                          sliderInput("lWidth", label="", min=0, max=1, value=.5, step=.01))
                  ),
                  
                  # Legend size slider
                  div(class="col-xs-4",
                      div(class="col-xs-12 radioText", "Legend size"),
                      div(class="col-xs-12 slider-input",
                          sliderInput("legendSize", label="", min=0, max=1, value=.5, step=.01))
                  )
                  
              )
             ), # withTags
             
             # Plot output
             h6(verbatimTextOutput("checkFile"),
                style="visibility: collapse; height: 0px;"),
             
             #conditionalPanel(
              #condition = "output.checkFile == '0'",
              #div(class="col-sm-12",
                  #uiOutput('message'),
                  #imageOutput("welcomeImage")
              #)
             #),
             
             conditionalPanel(
              "output.checkFile == '1'",
              plotOutput("plot", width = "100%", height = "100%")
             )
             
              #withTags(
              	#div(class='row',
              		#uiOutput('message'),
              		#div(class = "col-sm-12", plotOutput('plot'))
              		#)
              	#)
             
    ), # tabpanel Curve
    
    tabPanel("Summary",
             withTags(					
              div(class = "col-sm-12",
                  h3(id="model-summary", "Model(s) summary", align="center"),
                  tableOutput('summary')
              )
             )
    ), 
<<<<<<< HEAD
    
    tabPanel("Content",
             withTags(
              div(class = "col-sm-12",
                  h3(id="content", "Data Content", align= "center"),
                  tableOutput('content')
              )
             )
    )
    
=======
>>>>>>> 424ddf657da18176d039623de76ff31814b5ff64
    
    tabPanel("Content",
             withTags(
              div(class = "col-sm-12",
                  h3(id="content", "Data Content", align= "center"),
                  tableOutput('content')
              )
             )
    )
    
   ) #tabsetPanel
  )#main panel
  )
 )
  