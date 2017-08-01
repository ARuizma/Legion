library(shiny)
library(shinyjs)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
library(DT)
library(minpack.lm)
library(plotly)
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
 
 output$hist <- renderPlotly({
  his <- df()
  xCol <- xCol()
  hist <- ggplot(data = his, aes(his[[yCol()]])) + geom_histogram(col = "darkblue", aes(fill = ..count..)) + 
   scale_fill_gradient("Count", low = "green", high = "red") +
   #geom_density() +
  labs( x = input$ycol)
  
  hist <- ggplotly(hist)
  
  #hist$x$layout$width <- 600
  #hist$x$layout$height <- 500
  #hist$width <- NULL
  #hist$height <- NULL
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
 
 output$plot <- renderPlotly({

  if(is.null(df()))
   
     return(NULL)
#####OBJECTSDEFINED#######
  compoundCol <- compoundCol()
  xCol <- xCol()
  yCol <- yCol()
  dat <- df()
  names(dat)[names(dat) == xCol] <- "x"
  names(dat)[names(dat) == yCol] <- "y"
  names(dat)[names(dat) == compoundCol] <- "Compound"
  xCol <- "x"
  yCol <- "y"
  compoundCol <- "Compound"
  df2 <- df2()#NPLRMODELS
  dat2<- dat2()#NLSMODELS
  DFunction <- DFunction()
  x <- log10(DFunction[["x"]])
  x <- seq(min(x), max(x), length.out = length(dat[[compoundCol]])/length(unique(dat[[compoundCol]])))
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
  
  levels(nplr_df[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  colnames(nls_df) <- as.character(c(compoundCol, xCol, yCol))
  
  nls_df[[compoundCol]] <- as.factor(nls_df[[compoundCol]])
  
  levels(nls_df[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  dat <- dat[order(match(dat[[compoundCol]], nls_df[[compoundCol]])),]

# browser()
  ######PLOTS##############
  try(plot <- ggplot(data = dat, aes(x = log10(x), y = y)) + 
   geom_point() +
   stat_summary(fun.y = mean, color = "yellow", aes(group = Compound), geom = "point", size = 1.25) +
   stat_summary(fun.data = mean_se, geom = "errorbar", size = 1.25) +
   facet_wrap(~Compound, scales = "free", dir = "v") +
   labs(x = input$xcol, y = input$ycol))
   
  
  
  nplr_plot <- geom_line(data = nplr_df, aes(x,y, colour = "NPLR"), show.legend = TRUE, size = 1.25)
  nls_plot <- geom_line(data = nls_df , aes(log10(x), y, colour = "NLS"), show.legend = TRUE, size = 1.25)

  if(input$nplr_checkbox ==TRUE) {
   plot <- plot + nplr_plot}
  if(input$nls_checkbox == TRUE) {
   plot <- plot + nls_plot}
 
  plot <- plot + scale_colour_manual(name = "Models",
                                     breaks = c("NPLR", "NLS"),
                                     values = c("NPLR" = "red", "NLS" = "green"),
                                     labels = c("NPLR", "NLS"))
  plot <- ggplotly(plot)
  plot$x$layout$width <- NULL
  plot$x$layout$height <- "500px"
  plot$width <- NULL
  plot$height <- NULL
  
  plot
  
  })

 output$summary <- DT::renderDataTable({
  
  if(is.null(df()))
   return(NULL)
  
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
   
   j <- as.character(sumi$value[["xInfl"]])
   
   j <- as.numeric(j)
   
   expxinfl <- 10^j

   val.parameters<- data.frame("NPLR", "", input$ycol, sumi$value[["params.bottom"]], sumi$value[["params.top"]],
                                sumi$value[["xInfl"]], sumi$value[["params.scal"]], sumi$value[["weightedGOF"]], expxinfl ,stringsAsFactors=FALSE)
   colnames(val.parameters)<-c("Method","Compound","Feature","min","max","LoggedX50","Hill","r2","Inflexion")
   fit.parameters.nplr<-rbind(fit.parameters.nplr, val.parameters)
  })
  try(for(i in dat2) {
   
   sp_i<- pki.app.s4s.get.starting.parameters(i)
   
   fit <- pki.app.s4s.CurveFitting.Fit.Logistic(i, sp_i)
   
   val.parameters<-data.frame("NLS", unique(i[[compoundCol]]),input$ycol,
                              fit$B,fit$A,log10(fit$C),fit$D,fit$RSquared,fit$C, stringsAsFactors=FALSE)
   colnames(val.parameters)<-c("Method", "Compound","Feature","min","max","LoggedX50","Hill","r2","Inflexion")
   fit.parameters.nls<-rbind(fit.parameters.nls, val.parameters)
  })

  fit.parameters.nplr$Compound <- as.factor(names(df2))
  
  fit.parameters.nplr[,c(4:8)]<- sapply(fit.parameters.nplr[,c(4:8)], as.character)
  
  fit.parameters.nplr[,c(4:8)]<- sapply(fit.parameters.nplr[,c(4:8)], as.numeric)

  mySummary.nplr<-fit.parameters.nplr
  
  mySummary.nls <- fit.parameters.nls
  
  mySummary.nplr[,4:9] <- round(mySummary.nplr[,4:9], 4) 
  
  mySummary.nls[,4:9] <- round(mySummary.nls[,4:9], 4) 
  
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
