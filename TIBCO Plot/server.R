library(shiny)
library(XLConnect)
library(shinyjs)
library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
source("helpers.R")

 
server = function(input, output, session) {
 
 df <- reactive ({
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
 
 output$content<-renderTable({df()})

#NLSTEST####
 
 
 output$plot <- renderPlot({


  if(is.null(input$file1))
     return(NULL)
  
  dat <- df()
  
 
  switch(input$plot_scaletype,
         
         normal = ggplot(dat, aes_q(x=as.name(input$xcol), y=as.name(input$ycol), colour = as.name(input$zcol))) +
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          geom_point() +
          stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line"),
         
         log =
          ggplot(dat, aes_q(x=as.name(input$xcol), (y=as.name(input$ycol)), colour = as.name(input$zcol))) + 
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_log10() + 
          geom_point() #+
          #geom_smooth(method = 'nls', formula = y ~ a + ((b-a) / (1 + ((x / c) ^ d))), method.args = list(start= list(a = a, b = b, c = c, d = d)), se = FALSE, inherit.aes = TRUE)
          #stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line")
  
         )
 
 })
}