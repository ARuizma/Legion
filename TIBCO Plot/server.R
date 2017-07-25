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
 
 ##################################NLS###########################################
 
 DFunction <- try(reactive({
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
  
 }))
 
 dat2 <- try(reactive({
  DFunction <- DFunction()
  if(is.null(DFunction))
   return(NULL)
  split(DFunction, DFunction$Compound)
  }))
 
 ##############################################################NPLR##################################################################
 
 logistic <- function(x, B, TT, scal, xmid, s){
  
  (B+(TT-B)/(1+10^(scal*(xmid - x)))^s)
 }
 
 df3 <- try(reactive({
  if(is.null(df()))
   return(NULL)
  df5 <- df()
  df5[[yCol]] <- convertToProp(df5[[yCol]], T0 = NULL, Ctrl = NULL)
 }))
 
 df2 <-  try(reactive({

  if(is.null(df()))
   return(NULL)
  dt <- dat2()
  models <- lapply(dt, function(tmp){
  x <- tmp[,"x"]
  y <- tmp[,"y"]
  if(!is.numeric(x) || !is.numeric(y))
   return(NULL)
  npars <- ifelse(input$npars == "all", "all", as.numeric(input$npars))
  try(nplr(x, y, npars = npars, useLog = TRUE, silent = TRUE))
 })
  models
 }))
 
 #####PLOT################################
 
 output$plot <- renderPlot({

  if(is.null(df()))
   
     return(NULL)
#####OBJECTSDEFINED#######
  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  dat <- df()
  df2 <- df2()#NPLRMODELS
  dat2<- dat2()#NLSMODELS
  DFunction <- DFunction()
  x <- log10(DFunction[["x"]])
  nplr_df <- data.frame()
  cont <- 0
  nls_df<-data.frame()

#####LOOPS####################
  
  try(for (i in df2){
  
  B <- getPar(i)$params$bottom
  TT <- getPar(i)$params$top
  xmid <- getPar(i)$params$xmid
  s <- getPar(i)$params$s
  scal <- getPar(i)$params$scal
  
  cont <- cont + 1
  
  for (n in x){
 
   w <- logistic(n, B, TT, scal, xmid, s)
   
   nplr_df <- rbind(nplr_df, c(cont, n, w))
  
  }})
  
  for(i in dat2) {
   
   sp_i<- pki.app.s4s.get.starting.parameters(i)
   
   fit <- pki.app.s4s.CurveFitting.Fit.Logistic(i, sp_i)
   
   x <- i[["x"]]
   
   yfit <- predict(fit$fit, newdata = x)
   
   gg2<- cbind(i[["Compound"]], x, yfit)
   
   nls_df <- rbind(nls_df, gg2)
   
  }
  
  ######DFARRANGEMENTS#####
 
  colnames(nplr_df) <- as.character(c(compoundCol, xCol, yCol))
  
  nplr_df[[compoundCol]] <- as.factor(nplr_df[[compoundCol]])
  
  levels(nplr_df[[compoundCol]]) <- levels(DFunction[[compoundCol]])
  
  colnames(nls_df) <- as.character(c(compoundCol, xCol, yCol))
  
  nls_df[[compoundCol]] <- as.factor(nls_df[[compoundCol]])
  
  levels(nls_df[[compoundCol]]) <- levels(DFunction[[compoundCol]])
  
  dat <- dat[order(match(dat[[input$zcol]], nls_df[[compoundCol]])),]
  browser()
  ######PLOTS##############
  try(plot <- ggplot(data = dat, aes(x = log10(dat[[input$xcol]]), y = dat[[input$ycol]], show.legend = FALSE)) + 
   geom_point() +
   stat_summary(fun.y = mean, color = "black", aes(group = input$zcol), show.legend = FALSE, geom = "point") +
   stat_summary(fun.data = mean_se, geom = "errorbar", show.legend = FALSE) +
   facet_wrap(~dat[[input$zcol]], scales = "free", dir = "v") +
   guides(color = FALSE) +
   labs(x = input$xcol, y = input$ycol))
  
  nplr_plot <- geom_line(data = nplr_df, aes(x,y, colour = "green"), show.legend = FALSE)
  nls_plot <- geom_line(data = nls_df, aes(log10(x), y, colour = "blue"), show.legend = FALSE)

  if(input$nplr_checkbox ==TRUE) {
   plot <- plot + nplr_plot}
  if(input$nls_checkbox == TRUE) {
   plot <- plot + nls_plot}
  
  plot
 
  })

 output$summary <- DT::renderDataTable({
  
  dat <- df()
  df2 <- df2()
  dat2 <- dat2()
  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  r2<-0
  fit.parameters.nplr <- data.frame(Compound=character(),
                               Feature=character(),
                               min=numeric(),
                               max=numeric(),
                               Inflexion=numeric(),
                               Hill=numeric(),
                               r2=numeric(),
                               stringsAsFactors=FALSE) 
  
  fit.parameters.nls <- data.frame(Compound=character(),
                                    Feature=character(),
                                    min=numeric(),
                                    max=numeric(),
                                    Inflexion=numeric(),
                                    Hill=numeric(),
                                    r2=numeric(),
                                    stringsAsFactors=FALSE) 
  
  if(is.null(df2))
   return(NULL)
  mySummary = list()
  
  try(for (i in df2) {
   sumi <- summary.nplr(i)

   val.parameters<- data.frame("", input$ycol, sumi$value[["params.bottom"]], sumi$value[["params.top"]],
                                sumi$value[["Log10(IC50)"]], sumi$value[["params.scal"]], sumi$value[["weightedGOF"]], sumi$value[["IC50"]] ,stringsAsFactors=FALSE)
   colnames(val.parameters)<-c("Compound","Feature","min","max","LoggedX50","Hill","r2","Inflexion")
   fit.parameters.nplr<-rbind(fit.parameters.nplr, val.parameters)
  })
  try(for(i in dat2) {
   
   sp_i<- pki.app.s4s.get.starting.parameters(i)
   
   fit <- pki.app.s4s.CurveFitting.Fit.Logistic(i, sp_i)
   
   val.parameters<-data.frame(unique(i[[compoundCol]]),input$ycol,
                              fit$B,fit$A,log10(fit$C),fit$D,fit$RSquared,fit$C, stringsAsFactors=FALSE)
   colnames(val.parameters)<-c("Compound","Feature","min","max","LoggedX50","Hill","r2","Inflexion")
   fit.parameters.nls<-rbind(fit.parameters.nls, val.parameters)
  })

  fit.parameters.nplr$Compound <- as.factor(names(df2))
  
  fit.parameters.nplr[,c(3:8)]<- sapply(fit.parameters.nplr[,c(3:8)], as.character)
  
  fit.parameters.nplr[,c(3:8)]<- sapply(fit.parameters.nplr[,c(3:8)], as.numeric)

  mySummary.nplr<-fit.parameters.nplr
  
  mySummary.nls <- fit.parameters.nls
  
  mySummary.nplr[,3:8] <- round(mySummary.nplr[,3:8], 3) 
  mySummary.nls[,3:8] <- round(mySummary.nls[,3:8], 3) 

  
  mySummary <- data.frame(stringsAsFactors = FALSE)

  if(input$nplr_checkbox ==TRUE) {
   mySummary <- rbind(mySummary.nplr)}
  if(input$nls_checkbox == TRUE) {
   mySummary <- rbind(mySummary.nls)}
  if((input$nls_checkbox == TRUE) & (input$nplr_checkbox == TRUE)) {
   mySummary <- do.call("rbind",list(mySummary.nplr, mySummary.nls))}
  
  try(DT::datatable(mySummary, options = list(scrollX = TRUE)))
 })
})
