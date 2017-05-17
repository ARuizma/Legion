

library(nplr)
library(XLConnect)
library(shinyjs)
library(colourpicker)
source("helpers.r")

shinyServer(function(input, output, session) {
 
 Input <- reactiveValues(data = matrix(),
                         cells = character()
 )
 observe({Input$data <- .getData(input$file1$datapath, input$header)
 Input$cells <- names(Input$data)
 })
 
 test <- reactive({
  if(is.null(Input$data))
   return(NULL)
  models <- lapply(Input$data, function(tmp){
   x <- tmp[,2]
   y <- tmp[,3]
   if(!is.numeric(x) || !is.numeric(y))
    return(NULL)
   if(input$props){
    y <- convertToProp(y, T0 = NULL, Ctrl = NULL)
   }
   npars <- ifelse(input$npar=="all", "all", as.numeric(input$npar))
   nplr(x, y, npars=npars, useLog=input$toLog, silent = TRUE)
  })
  models
 })
 
 checkFile <- reactive({
  if(is.null(input$file1$datapath))
   return(0)
  return(1)
 })
 
 
 output$checkFile <- renderText({ checkFile() })
 
 output$modelNames <- renderUI({
  models <- test()
  if(length(models)<1)
   return(NULL)
  else{
   items <- names(models)
   withTags(
    div(class="row",
        div(class="col-xs-12 radioText", "Set colors", style="margin-left: 15px; "),
        div(class="col-xs-12",
            lapply(.renderDiv(items), function(x) eval(parse(text=x)) )
        )
    )
   )
  }
 })
 
 # Put all the input colors in a vector
 getColors <- reactive({
  models <- test()
  items <- names(models)
  cols <- lapply(seq_len(length(items)), function(i) {
   input[[paste("col", i, sep="_")]]
  })
  unlist(cols)
 })
 
 output$plot <- renderPlot({
  if(is.null(input$file1$datapath))
   exampleInput()
  if(is.null(test()))
   return(NULL)
  
  models <- test()
  
  .multiCurve(models,
              showPoints = input$points,
              showMeans = input$Means,
              showSDerr = input$SDerr,
              pSize = input$pSize,
              lWidth = input$lWidth,
              legendSize = input$legendSize,
              showAsLog = input$showAsLog,
              Legend = input$showLegend,
              Cols = getColors(),
              xlab=input$xlabel, ylab=input$ylabel,
              las = 1
  )
  
 }, res=180, width=1033, height=875)
 
 
 session$onSessionEnded(function() { stopApp() })
 
})