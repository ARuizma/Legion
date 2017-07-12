library(shiny)
library(XLConnect)
library(shinyjs)
library(colourpicker)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
library(minpack.lm)
source("helpers.R")

 
server = function(input, output, session) {
 
 df <- reactive ({
  infile <- input$file1
  if(is.null(infile))
   return(NULL)
 
 df<- read.csv(infile$datapath, header=input$header, sep=input$sep, check.names = FALSE)
 
 observe({
  updateSelectInput(session, 'zcol', choices = names(df), selected=names(df)[1])
  "x" <- updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
  "y" <- updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
 })
 
 return(df)
 
 })
 
 output$content<-renderTable({df()})

#NLSTEST####
 
 compoundCol <- reactive({
  input$zcol
 })
 
 xCol <- reactive({
  input$xcol
 })
 
 
 yCol <- reactive ({
  input$ycol
 })
 
 output$plot <- renderPlot({


  if(is.null(input$file1))
     return(NULL)
  form <- y ~ a + ((b-a) / (1 + ((x/c)^d)))
  dat <- df()
  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  names(dat)[names(dat) == xCol] <- "x"
  names(dat)[names(dat) == yCol] <- "y"
  names(dat)[names(dat) == compoundCol] <- "Compound"
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  dat[[xCol]] <- log10(dat[[xCol]])
  dat[[xCol]][dat[[xCol]] == -Inf] <- NULL

  datos <- pki.app.s4s.get.starting.parameters(dat)
  
  p2 <-   ggplot(dat, aes(x = x, y=y, color = Compound)) + 
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          geom_point() 
          
          
 
  p2 <- p2 + geom_smooth(data = dat, method = 'nlsLM', formula = form, method.args = list(datos), se = FALSE) +
  stat_summary(fun.y = mean, geom="point", color = "yellow", aes(group=Compound), show.legend = FALSE) +
  facet_wrap(~Compound, scales = "free_y", dir = "v") +
  labs( x = "Concentration", y = "Data", colour = compoundCol) 
  
  
  plot(p2)       
 
 })
}