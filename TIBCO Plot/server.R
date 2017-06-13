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
 
 df<- read.csv(infile$datapath, header=input$header, sep=input$sep, quote= input$quote, check.names = FALSE)
 
 observe({
  x <- updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
  y <- updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({data()})
  
 
 output$plot <- renderPlot({
  
  if(is.null(input$file1))
     return(NULL)
  
  df <- data()[, c(input$xcol, input$ycol)]
  switch(input$plot_scaletype,
         normal = plot(df),
         log =
          plot(df, log = "xy", type = "p", main = "Curve Fitting")
  )
  
 })
 
}
 




