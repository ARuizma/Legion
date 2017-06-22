library(shiny)
library(XLConnect)
#library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
source("helpersnplr.R")

#options(shiny.trace =TRUE)
 
server = function(input, output, session) {

 data <- reactive ({
  infile <- input$file1
  if(is.null(infile))
   return(NULL)
 
 df<- read.table(infile$datapath, header=input$header, sep=input$sep, check.names = FALSE)
 
 observe({
updateSelectInput(session, 'zcol', choices = names(df), selected=names(df)[1])
updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({data()})
 

 #NPLRTEST####
 
df2<- reactive ({
 names<- data()[,input$zcol]
})



datalist <- reactive({
 if(is.null(data()))
    return(NULL)
 split(data(), df2())
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
browser()
  nplr((x+0.0001), y, npars = "all", useLog = input$toLog, silent = TRUE)
 })

})

 output$plot <- renderPlot({
  
  if(is.null(test()))
     return(NULL)
 .multiCurve(test(),
             las = 1
 )
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

 
 output$summary <- renderTable({
  if(is.null(test()))
   return(NULL)
  buildSummary(test())
 })
 
 output$test <- renderTable({
 
 print((log(datalist()))
 )
 }
 )
 
}


