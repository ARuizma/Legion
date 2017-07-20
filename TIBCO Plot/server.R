library(shiny)
library(shinyjs)
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
  updateSelectInput(session, 'xcol', choices = names(df), selected=names(df)[2])
  updateSelectInput(session, 'ycol', choices = names(df), selected=names(df)[3])
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
 
 DFunction <- reactive({
  if(is.null(input$file1))
   return(NULL)
  
  DFunction <- df()
  xCol <- xCol()
  yCol <- yCol()
  compoundCol <- compoundCol()
  names(DFunction)[names(DFunction) == xCol] <- "x"
  names(DFunction)[names(DFunction) == yCol] <- "y"
  names(DFunction)[names(DFunction) == compoundCol] <- "Compound"
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  
  if (length(DFunction[[xCol]] > 0)) {
   DFunction <- aggregate(y~Compound + x, DFunction, mean)
  }
 
  
  DFunction <- DFunction[complete.cases(DFunction),]
  
  DFunction <- DFunction[DFunction[, "x"] > 0,]
  
  DFunction <- unique(DFunction)
  
  return(DFunction)
  })
 
 dat2 <- reactive({
  DFunction <- DFunction()
  compoundCol <- compoundCol()
 if(is.null(DFunction))
  return(NULL)

  split(DFunction, DFunction$Compound)
 })
 
 output$plot <- renderPlot({
  
  if(is.null(input$file1))
     return(NULL)

  dat <- df()
  dat2<- dat2()
  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  names(dat)[names(dat) == xCol] <- "x"
  names(dat)[names(dat) == yCol] <- "y"
  names(dat)[names(dat) == compoundCol] <- "Compound"
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  gg<-data.frame()
  
  for(i in dat2) {
   
   sp_i<- pki.app.s4s.get.starting.parameters(i)
   
   fit <- pki.app.s4s.CurveFitting.Fit.Logistic(i, sp_i)
   
   x <- i[[xCol]]
   
   yfit <- predict(fit$fit, newdata = x)
   
   gg2<- cbind(i[[compoundCol]], x, yfit)
   
   gg <- rbind(gg, gg2)
   
  }
  
  colnames(gg) <- as.character(c(compoundCol, xCol, yCol))
  
  gg[[compoundCol]] <- as.factor(gg[[compoundCol]])
  
  levels(gg[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  dat <- dat[order(match(dat[[compoundCol]], gg[[compoundCol]])),]
  
  p2 <-   ggplot(dat, aes(x = log10(x), y=y, colour = Compound)) + 
          ggtitle("Curve Fitting") + theme(plot.title = element_text(hjust = 0.5)) +
          geom_point() 
          
  p2 <- p2 + 
  #geom_smooth(data = dat, method = 'nlsLM', formula = form, method.args = list(datos), se = FALSE) +
  geom_line(data = gg, aes(log10(x), y)) +
  stat_summary(fun.y = mean, geom="point", colour = "yellow", aes(group=Compound), show.legend = FALSE) +
  facet_wrap(~Compound, scales = "free", dir = "v") +
  labs( x = "Concentration", y = "Data", colour = compoundCol) 
  
 p2
 
 })


output$summary <- renderTable({
 if(is.null(input$file1))
  return(NULL)
 
 dat <- df()
 dat2<- dat2()
 compoundCol <- compoundCol()
 xCol <- xCol()
 yCol <- yCol()
 xCol <- "x"
 yCol <- "y"
 compoundCol <- "Compound"
 r2<-0
 fit.parameters <- data.frame(Compound=character(),
                              Feature=character(),
                              min=numeric(),
                              max=numeric(),
                              Inflexion=numeric(),
                              Hill=numeric(),
                              r2=numeric(),
                              Notes=character(), 
                              stringsAsFactors=FALSE) 
 browser()
 for(i in dat2) {
  
  sp_i<- pki.app.s4s.get.starting.parameters(i)
  
  fit <- pki.app.s4s.CurveFitting.Fit.Logistic(i, sp_i)
  
  val.parameters<-data.frame(unique(i[[compoundCol]]),input$ycol,
                             fit$B,fit$A,log10(fit$C),fit$D,fit$RSquared,fit$C,
                             fit$FitResult)
  colnames(val.parameters)<-c("Compound","Feature","min","max","LoggedX50","Hill","r2","Inflexion","Notes")
  fit.parameters<-rbind(fit.parameters, val.parameters)
  
  # Generate the final result
  r2<-fit$RSquared
  newMin<-fit$B
  newMax<-fit$A
  newSlope<-fit$D
 }
 
 pki.app.s4s.fit.parameters<-fit.parameters
 
 fit.parameters
})
}
 