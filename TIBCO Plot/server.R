library(shiny)
library(XLConnect)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
library(DT)
library(minpack.lm)
source("helpers.R")

shinyServer(function(input, output, session) {

#########################GENERAL#########################################################
  
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
 
 output$content<-DT::renderDataTable({
  
  cont <- df()
  
  DT::datatable(cont, options = list(scrollX = TRUE, scrollY = TRUE))})
 
 compoundCol <- reactive({
  input$zcol
 })
 
 xCol <- reactive({
  input$xcol
 })
 
 yCol <- reactive ({
  input$ycol
 })
 
 ##############################################################NPLR##################################################################
 
 logistic <- function(x, B, TT, scal, xmid, s){
  
  (B+(TT-B)/(1+10^(scal*(xmid - x)))^s)
 }
 
 var <- reactive({
  xCol <- xCol()
  dat <- df()
 dat[[xCol]][dat[[xCol]]==0] <- NA
 (min(dat[[xCol]], na.rm = TRUE)/2)
 })
  
 
 df3 <- try(reactive({
  if(is.null(df()))
   return(NULL)
  df5 <- df()
  df5[[yCol]] <- convertToProp(df5[[yCol]], T0 = NULL, Ctrl = NULL)
 }))
 
 datalist<- reactive ({
  df()[,input$zcol]
 })
 
 datalist2 <- reactive({
  split(df(), datalist())
 })
 
 df2 <-  try(reactive({
  if(is.null(df()))
   return(NULL)
  dt <- datalist2()
  models <- lapply(datalist2(), function(tmp){
  x <- log10(tmp[,xCol()])
  y <- tmp[,yCol()]
  x[x == -Inf] <- log10(var())
  if(!is.numeric(x) || !is.numeric(y))
   return(NULL)
  npars <- ifelse(input$npars == "all", "all", as.numeric(input$npars))
  try(nplr(x, y, npars = npars, useLog = FALSE, silent = TRUE))
 })
  models
 }))
 
 output$nplrplot <- renderPlot({

  if(is.null(df()))
   
     return(NULL)

  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  df2 <- df2()
  dat <- df()
  var <- var()
  
  dat[[xCol]] <- log10(dat[[xCol]])
  
  dat[[xCol]][dat[[xCol]] == -Inf] <- log10(var)
 
  x <- dat[[xCol]]
  
  x <- seq(min(x), max(x), length.out = length(dat[[compoundCol]])/length(unique(dat[[compoundCol]])))
  gg <- data.frame()
  cont <- 0
  
  try(for (i in df2){
  
  B <- getPar(i)$params$bottom
  TT <- getPar(i)$params$top
  xmid <- getPar(i)$params$xmid
  s <- getPar(i)$params$s
  scal <- getPar(i)$params$scal
  
  cont <- cont + 1
  
  for (n in x){
 
   w <- logistic(n, B, TT, scal, xmid, s)
   
   gg <- rbind(gg, c(cont, n, w))
   
   colnames(gg) <- as.character(c(compoundCol, xCol, yCol))
  
  }})
  
  gg[[compoundCol]] <- as.factor(gg[[compoundCol]])
  
  levels(gg[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  dat2 <- dat[order(match(dat[[compoundCol]], gg[[compoundCol]])),]
  
  p1 <- ggplot(data = dat2, aes(x=dat2[[xCol]], y=dat2[[yCol]], colour = gg[[compoundCol]]), show.legend = FALSE) + 
   geom_point() 

  p1 <- try(p1 + geom_line(data = gg, aes(x = gg[[xCol]], y = gg[[yCol]], colour = gg[[compoundCol]]), show.legend = FALSE) +
   stat_summary(fun.y = mean, color = "black", aes(group = dat2[[compoundCol]]), show.legend = FALSE, geom = "point") +
   stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
   facet_wrap(~gg[[compoundCol]], scales = "free", dir = "v") +
   guides(color = FALSE) +
   labs(x = input$xcol, y = input$ycol))
  
  try(plot(p1))
  
  })

 output$nplrsummary <- DT::renderDataTable({
  
  dat <- df()
  df2 <- df2()
  compoundCol <- compoundCol()
  
  if(is.null(df2))
   return(NULL)
  mySummary = list()
  
  for (i in df2) {
   sumi <- summary.nplr(i)
   mySummary <- append(mySummary, sumi)
  }
  
  mySummary <- as.data.frame(mySummary)
  
  names(mySummary) <- levels(dat[[compoundCol]])
  
  mySummary <- t(mySummary)
  
  DT::datatable(mySummary, options = list(scrollX = TRUE))
  
 })
 
##################################NLS###########################################

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

output$nlsplot <- renderPlot({
 
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
  geom_point() 
 
 p2 <- p2 + 
  geom_line(data = gg, aes(log10(x), y, colour = Compound), show.legend = FALSE) +
  stat_summary(fun.y = mean, geom="point", colour = "black", aes(group=Compound), show.legend = FALSE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
  facet_wrap(~Compound, scales = "free", dir = "v") +
  guides(color = FALSE) +
  labs(x = input$xcol, y = input$ycol)
 
 p2
 
})


output$nlssummary <- DT::renderDataTable({
 
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
 
 DT::datatable(fit.parameters, options = list(scrollX = TRUE))
})
})
