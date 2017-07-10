library(shiny)
library(XLConnect)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)



shinyServer(function(input, output, session) {
 
 options(warn = -1)
 
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
 
 
 logistic <- function(x, B, TT, scal, xmid, s){
  
  (B+(TT-B)/(1+10^(scal*(xmid - x)))^s)
 }
 
 
 
compoundCol <- reactive({
 input$zcol
})
 
 xCol <- reactive({
  input$xcol
 })
 
 xLog <- reactive({
  df <- df()
  log10(df[[xCol()]])
 })
 
 yCol <- reactive ({
  input$ycol
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
  x[x == -Inf] <- 0
  if(!is.numeric(x) || !is.numeric(y))
   return(NULL)
  npars <- ifelse(input$npars == "all", "all", as.numeric(input$npars))
  try(nplr(x, y, npars = npars, useLog = FALSE, silent = TRUE))
 })
  models
 }))
 
 
 output$plot <- renderPlot({
  
  
  
  if(is.null(df()))
   
     return(NULL)

  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  df2 <- df2()
  dat <- df()
  
  dat[[xCol]] <- log10(dat[[xCol]])
  
  dat[[xCol]][dat[[xCol]] == -Inf] <- 0
 
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
   stat_summary(fun.y = mean, color = "yellow", aes(group = dat2[[compoundCol]]), show.legend = FALSE) +
   stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
   facet_grid(gg[[compoundCol]]~., scales = "free_y") +
   labs(x = xCol, y = yCol, colour = compoundCol))
  
  try(plot(p1))
  
  })

 
 output$summary <- renderTable({
  
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

  names(mySummary) <- levels(dat[[compoundCol]])
  
  mySummary

 }, include.rownames = TRUE)
 })
 


 