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
 
 #NPLRTEST####

 df2<- reactive ({
  df()[,input$zcol]
 })

 output$plot <- renderPlot({
 
  if(is.null(df()))
   
     return(NULL)
  
  dat <- df()
  dat[input$xcol] <- log10(dat[input$xcol])
  
  
  #dat_1 <- dat[dat$cell == "easyRider",]
  npars <- ifelse(input$npars == "all", "all", as.numeric(input$npars))
  prueba <- nplr(x = as.numeric(unlist(dat[input$xcol])), y = as.numeric(unlist(dat[input$ycol])), npars=npars, useLog=FALSE)
  B <- getPar(prueba)$params$bottom
  TT <- getPar(prueba)$params$top
  xmid <- getPar(prueba)$params$xmid
  s <- getPar(prueba)$params$s
  scal <- getPar(prueba)$params$scal
  logistic <- function(x){
   
   (B+(TT-B)/(1+10^(scal*(xmid - x)))^s)
  }
  #browser()
  p1 <- ggplot(data = dat, aes_q(x=as.name(input$xcol), y=as.name(input$ycol), colour = as.name(input$zcol))) + geom_point() + facet_wrap(~df2(), scales = "free")
  
  for (i in 1:df2()) {

  p1 <- p1 + stat_function(fun = logistic, colour = "black")
  }
  
  #dat_1 <- dat[dat$cell == "Hurrycane",]
  #prueba <- nplr(x = as.numeric(unlist(dat_1[input$xcol])), y = as.numeric(unlist(dat_1[input$ycol])), npars="all", useLog=FALSE)
  #B1 <- getPar(prueba)$params$bottom
  #TT1 <- getPar(prueba)$params$top
  #xmid1 <- getPar(prueba)$params$xmid
  #s1 <- getPar(prueba)$params$s
  #scal1 <- getPar(prueba)$params$scal
  
  #logistic_1 <- function(x){
   
   #(B1+(TT1-B1)/(1+10^(scal1*(xmid1 - x)))^s1)
  #}
  
  #p1 <- p1 + stat_function(fun = logistic_1, colour = as.name(input$zcol))
  
  plot(p1)
  })

 
 #+ facet_wrap(~input$zcol, scales = "free") + stat_summary(fun.y = "mean", colour = "Red", geom = "point", size = 5) 

 #output$summary <- renderTable({
  #models <- data()
  #if(is.null(models))
   #return(NULL)
  #buildSummary(models)
 #})
 
})
 