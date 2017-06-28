library(shiny)
library(XLConnect)
library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)



options(shiny.error =browser)
#options(shiny.error =recover)
 
shinyServer(function(input, output, session) {

 df <- reactive ({
  infile <- input$file1
  if(is.null(infile))
   return(NULL)
 
 df<- read.csv(infile$datapath, header=input$header, sep=input$sep, check.names = FALSE)
 
 observe({
updateSelectInput(session, 'zcol', choices = names(df), selected=names(df)[1])
updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({df()})
 
#browser()
 #NPLRTEST####

 df2<- reactive ({
  df()[,input$zcol]
 })
 
 datalist <- reactive({
  if(is.null(df()))
   return(NULL)
  split(df(), df2())
 })
 
 test <- reactive({
  if(is.null(datalist()))
   return(NULL)
  models <- lapply(datalist(), function(tmp){
   x <- tmp[,input$xcol]
   y <- tmp[,input$ycol]
   if(!is.numeric(x) || !is.numeric(y))
    return(NULL)
   if(input$props){
    y <- convertToProp(y, T0=NULL, Ctrl = NULL)
   }
    nplr(x, y, npars = "all", useLog = input$toLog, silent=TRUE)
   
  })
  models
 })



B <- reactive({
 models<-test()
 lapply(models, function(model){
  
 getPar(model)$params$bottom
 })
})

TT <- reactive({
 models<-test()
 lapply(models, function(model){
  
  getPar(model)$params$top
 })
})

xmid <- reactive({
 models<-test()
 lapply(models, function(model){
  
  getPar(model)$params$xmid
 })
})

s <- reactive({
 models<-test()
 lapply(models, function(model){
  
  getPar(model)$params$s
 })
})

scal <- reactive({
 models<-test()
 lapply(models, function(model){
  
  getPar(model)$params$scal
 })
})
 

 
 logistic <- function(x){
  
  (B()+(TT()-B())/(1+10^(scal()*(xmid() - x)))^s())
 }
 
 output$plot <- renderPlot({
 
  if(is.null(df()))
   
     return(NULL)
  
  dat <- df()
  browser()
  ggplot(dat, aes(x=dat[input$xcol], y=dat[input$ycol])) + geom_point(colo#stat_function(fun = logistic()) #+ facet_wrap(~input$zcol, scales = "free") + stat_summary(fun.y = "mean", colour = "Red", geom = "point", size = 5) #geom_point(aes(colour = Compound))
 })


 #output$summary <- renderTable({
  #models <- data()
  #if(is.null(models))
   #return(NULL)
  #buildSummary(models)
 #})
 
})
 