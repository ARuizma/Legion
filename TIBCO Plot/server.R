library(shiny)
library(XLConnect)
library(shinyjs)
library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
#source("helpersnls")
 
server = function(input, output, session) {
 
 data <- reactive ({
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
 
 output$content<-renderTable({data()})
 
 #NPLRTEST####
 
 datalist <- reactive ({
 if (is.null(infile))
     return(NULL)
 filelist <- split(infile, input$zcol)
 })
 
 conc <- input$xcol
 y0 <- input$ycol
 y1 <- convertToProp(y0)

 
 test <- 
 models <- lapply(datalist, function(tmp){
  nplr(conc, y1, useLog = TRUE, LPweight = 0.25, npars="all", method = c("res", "sdw", "gw"), silent = FALSE)
 })
 
 output$plot <- renderPlot({
  
  if(is.null(input$file1))
     return(NULL)
  
  plot(models)
  #dat <- data()
  
 # switch(input$plot_scaletype,
         
  #       normal = ggplot(dat, aes_q(x=as.name(input$xcol), y=as.name(input$ycol), colour = as.name(input$zcol))) +
   #       ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
    #      geom_point() +
     #     stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line"),
         
      #   log =
       #   ggplot(dat, aes_q(x=as.name(input$xcol), (y=as.name(input$ycol)), colour = as.name(input$zcol))) + 
        #  ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
         # scale_x_log10() + 
          #geom_point() +
          #geom_smooth(method = 'nplr', formula = , method.args = list(start = c(a = "a2", b = "b2", c = "c2", d = "d2")), se = FALSE)
          #stat_summary(aes_q(y=as.name(input$ycol), group=as.name(input$zcol), colour = as.name(input$zcol)), fun.y = mean, geom = "line")
  #)
 })
 
 output$summary <- renderPrint({
  
  if(is.null(input$file1))
   return(NULL)
  
  summary(models)
 })
}