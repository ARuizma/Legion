library(shiny)
library(XLConnect)
library(shinyjs)
library(colourpicker)
library(nlstools)
library(ggplot2)


 
server = function(input, output, session) {
 
 data <- reactive ({
  infile <- input$file1
  if(is.null(infile))
   return(NULL)
 
 df<- read.csv(infile$datapath, header=input$header, sep=input$sep, quote= input$quote)
 
 observe({
  updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
  updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({data()})
  
 
 #selectedData <- reactive({
  #upfile[, c(input$xcol, input$ycol)]
 #})
 
 output$plot <- renderPlot({
  
  x <- data()[, c(input$xcol, input$ycol)]
  switch(input$plot_scaletype,
         normal = plot(x),
         log =
          plot(x, log = "xy", type = "l", main = "Curve Fitting")
  )
  
 })
 
}
 




