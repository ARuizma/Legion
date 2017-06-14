library(shiny)
library(XLConnect)
library(shinyjs)
library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
 
server = function(input, output, session) {
 
 data <- reactive ({
  infile <- input$file1
  if(is.null(infile))
   return(NULL)
 
 df<- read.csv(infile$datapath, header=input$header, sep=input$sep, check.names = FALSE)
 
 observe({
  z <- updateSelectInput(session, 'zcol', choices = names(df), selected=names(df)[1])
  x <- updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
  y <- updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({data()})
  
 
 output$plot <- renderPlot({
  
  if(is.null(input$file1))
     return(NULL)
  
  dat <- data()
  
 # xy <- data()[, c(input$xcol, input$ycol)]
  switch(input$plot_scaletype,
         
         normal = ggplot(dat, aes_q(x=as.name(input$xcol), y=as.name(input$ycol), colour = as.name(input$zcol))) +
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          geom_point() +
          stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line"),
         
         log =
          ggplot(dat, aes_q(x=as.name(input$xcol), (y=as.name(input$ycol)), colour = as.name(input$zcol))) + 
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_log10() + 
          geom_point() +
          stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line")
  )
 })
 
 output$summary <- renderPrint({
  
  if(is.null(input$file1))
   return(NULL)
  
  summary(data())
 })
}