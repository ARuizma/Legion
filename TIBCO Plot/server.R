library(shiny)
library(XLConnect)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
library(DT)


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
 
 output$content<-DT::renderDataTable({
  
  cont <- df()
  
  DT::datatable(cont, options = list(scrollX = TRUE, scrollY = TRUE))})
 
 #NPLR####
 
 logistic <- function(x, B, TT, scal, xmid, s){
  
  (B+(TT-B)/(1+10^(scal*(xmid - x)))^s)
 }

compoundCol <- reactive({
 input$zcol
})

 xCol <- reactive({
  input$xcol
 })

 yCol <- reactive ({
  input$ycol
 })
 
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
  browser()
  p1 <- try(p1 + geom_line(data = gg, aes(x = gg[[xCol]], y = gg[[yCol]], colour = gg[[compoundCol]]), show.legend = FALSE) +
   stat_summary(fun.y = mean, color = "yellow", aes(group = dat2[[compoundCol]]), show.legend = FALSE) +
   stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
   facet_wrap(~gg[[compoundCol]], scales = "free_y", dir = "v") +
   guides(color = FALSE) +
   labs(x = xCol, y = yCol))
  
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
 })