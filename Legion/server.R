library(shiny)
library(shinyjs)
library(nlstools)
library(ggplot2)
library(shinydashboard)
library(nplr)
library(DT)
library(minpack.lm)
library(plotly)
library(Rtsne)
library(RColorBrewer)
library(fpc)
source("helpers.R")

options(shiny.maxRequestSize = 30*1024^2)

parse_widgets <- function(conf_path) {
 
 my_list_of_widgets <- fromJSON(conf_path)
 
 id_of_widgets <- names(my_list_of_widgets)
 
 list_of_tags <- c()
 
 for (widget_id in id_of_widgets) {
  
  widget_parameters <- names(my_list_of_widgets[[widget_id]])
  
  list <- c()
  
  for (key in widget_parameters) {
   
   if(key != "type") {
    
    value = my_list_of_widgets[[widget_id]][[key]]
    if (grepl('%LIST_SEPARATOR%', value)) {
     value = gsub('%LIST_SEPARATOR%', "', '", value)
     value = paste('c(\'', value, '\')', sep = '')
    }
    
    list <- c(list, paste(key, value, sep = ' = '))
   }
  }
  list <- c(list, paste("'inputId'", paste('\'', widget_id, '\'', sep = ''), sep = ' = '))
  
  tag = paste(my_list_of_widgets[[widget_id]][['type']], '(', sep = '')
  tag_key_values = paste0(list, collapse = ', ')
  tag = paste(tag, tag_key_values, ')', sep = '')
  
  list_of_tags <- c(list_of_tags, tag)
 }
 
 list_of_tags
 
}

shinyServer(function(input, output, session) {

 configuration_path <- "C:\\Users\\Capitán Tomate\\Documents\\GitHub\\Atlassian\\TIBCO Plot\\conf.json";
 widgets <- parse_widgets(conf_path = configuration_path)
 output$widget <- renderUI({
  tagList(lapply(widgets, function(x) { eval(parse(text=x)) }))
 })
 
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
   updateSelectInput(session, 'ccol', choices = names(df))
   updateSelectInput(session, 'dcol', choices = names(df))
   updateSelectInput(session, 'colcol', choices = names(df))
   updateSelectInput(session, 'axcol', choices = names(df))
   updateSelectInput(session, 'aycol', choices = names(df))
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
 
 cCol <- reactive ({
  input$ccol
 })
 
 dCol <- reactive ({
  input$dcol
 })
 
 colCol <- reactive ({
  input$colcol
 })
 
 axCol <- reactive ({
  input$axcol
 })
 
 ayCol <- reactive ({
  input$aycol
 })
 
 #output$hist <- renderPlotly({
 #his <- df()
 #xCol <- xCol()
 #hist <- ggplot(data = his, aes(his[[yCol()]])) + geom_histogram(col = "darkblue", aes(fill = ..count..)) + 
 #scale_fill_gradient("Count", low = "green", high = "red") +
 #labs( x = input$ycol)
 
 #hist <- ggplotly(hist)
 
 #hist$x$layout$width <- NULL
 #hist$x$layout$height <- NULL
 #hist$width <- NULL
 #hist$height <- NULL
 #hist
 #})
 
 #CURVEFITTING####
 
 #NLS####
 
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
 
 #NPLR####
 
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
 
#####UIOUTPUT####
 
 output$npars <- renderUI({
  if(input$nplr_checkbox == TRUE)
   npars =  selectInput('npars', 'Number of Parameters', c("Best" = 'all', "2" = '2', "3" = '3', "4"= '4', "5"='5'), "all")})
 output$perp <- renderUI({
  if(input$tsne_checkbox == TRUE)
   perp = sliderInput('perp', 'Perplexity', min=1, max=100, value = 30)
 })
 output$alg <- renderUI({
  if(input$kmeans_checkbox == TRUE)
   selectInput('alg', 'Method', c('Hartigan-Wong', "Lloyd", "Forgy", "MacQueen"), "Hartigan-Wong")
 })
 output$met <- renderUI({
  if(input$hieclu_checkbox == TRUE)
  selectInput('met', 'Method', c("single", "complete", "average", "mcquitty", "median", "centroid"), "complete")
 })
 output$grad <- renderUI({
  if(input$dim == 3)
   checkboxInput('grad', 'Show as Gradient', value = FALSE)
 })
 #PLOT####
 
 output$plot <- renderPlotly({
  
  if(is.null(df()))
   
   return(NULL)
  #OBJECTSDEFINED####
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
  
  #LOOPS####
  
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
  
  #DFARRANGEMENTS####
  
  colnames(nplr_df) <- as.character(c(compoundCol, xCol, yCol))
  
  nplr_df[[compoundCol]] <- as.factor(nplr_df[[compoundCol]])
  
  levels(nplr_df[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  colnames(nls_df) <- as.character(c(compoundCol, xCol, yCol))
  
  nls_df[[compoundCol]] <- as.factor(nls_df[[compoundCol]])
  
  levels(nls_df[[compoundCol]]) <- levels(dat[[compoundCol]])
  
  dat <- dat[order(match(dat[[compoundCol]], nls_df[[compoundCol]])),]
  
  #PLOTS####
  
  try(plot <- ggplot(data = dat, aes(x = log10(x), y = y)) + 
       geom_point() +
       stat_summary(fun.y = mean, color = "yellow", aes(group = Compound), geom = "point", size = 1.25) +
       stat_summary(fun.data = mean_se, geom = "errorbar", size = 1.25) +
       facet_wrap(~Compound, scales = "free", dir = "v") +
       labs(x = input$xcol, y = input$ycol))
  
  
  
  nplr_plot <- geom_line(data = nplr_df, aes(x,y, colour = "NPLR"), show.legend = TRUE, size = 0.75)
  nls_plot <- geom_line(data = nls_df , aes(log10(x), y, colour = "NLS"), show.legend = TRUE, size = 0.75)
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
  
  mySummarydef <- try(DT::datatable(mySummary, options = list(scrollX = TRUE, order = list(list(8, 'desc')))))
  
 })
 
 output$down <- downloadHandler('Curve-Fitting-Summary.csv', 
   content <- function(file) {
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
    
    mySummary
   write.csv(mySummary, file, sep ='', row.names = FALSE)})
 
 #DIMENSIONALITYREDUCTION####
 
 output$tsne <- renderPlotly({
  if(is.null(df()))
   return(NULL)
  dCol <- dCol()
  dat <- df()
  colCol <- colCol()
  dt <- unique(dat)
  lst <- lapply(dt[dCol], function(x) (x-min(x))/(max(x)-min(x)))
  cc <- as.factor(dat[[colCol]])
  df <- data.frame()
  
  res <- rbind(df, lst)
  tsne <- Rtsne(res, dims = as.numeric(input$dim), perplexity=input$perp, verbose=TRUE, max_iter = 500, check_duplicates = FALSE)
  if(input$dim == 2) {
  tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2])
  tsnep <- ggplot(tsne_plot) + geom_point(aes(x=x, y = y, color = dt[[colCol]])) + labs(color = colCol, x = "X", y = "Y")
  tsnep <- ggplotly(tsnep)
  }
  if(input$dim == 3) {
   tsne_plot <- data.frame(x = tsne$Y[,1], y = tsne$Y[,2], z = tsne$Y[,3])
   if(input$grad == TRUE) {
   tsnep <- plot_ly(tsne_plot, x = ~tsne$Y[,1], y = ~tsne$Y[,2], z = ~tsne$Y[,3], marker = list(color = ~cc, colorscale = c('#BF382A', '#0C4B8E'), showscale = TRUE), width = NULL) %>%
   add_markers() %>% layout(scene = list(xaxis = list(title = "X"), yaxis = list(title = "Y"), zaxis = list(title = "Z"))) }
   if(input$grad == FALSE) {
   tsnep <- plot_ly(tsne_plot, x = ~tsne$Y[,1], y = ~tsne$Y[,2], z = ~tsne$Y[,3], color = ~cc, colors = c('#BF382A', '#0C4B8E'), width = NULL) %>%
    add_markers() %>% layout(scene = list(xaxis = list(title = "X"), yaxis = list(title = "Y"), zaxis = list(title = "Z")))}
  }
  tsnep$x$layout$width <- NULL
  tsnep$x$layout$height <- NULL
  tsnep$width <- NULL
  tsnep$height <- NULL
  
  if(input$tsne_checkbox == TRUE) {
   tsnep}
 })
 
 output$pca <- renderPlotly({
  if(is.null(df()))
   return(NULL)
  dCol <- dCol()
  colCol <- colCol()
  dat <- df()
  cc <- as.factor(dat[[colCol]])
  lst <- lapply(dat[dCol], function(x) (x-min(x))/(max(x)-min(x)))
  df <- data.frame()
  res <- rbind(df, lst)
  pc <- prcomp(res)
  if(input$dim == 2) {
   pcap <- ggplot(res) + geom_point(aes(x = pc$x[,1], y = pc$x[,2], color = dat[[colCol]])) + labs(color = colCol, x = "X", y = "Y")
   pcap <- ggplotly(pcap)}
  if(input$dim == 3) {
   if(input$grad == FALSE) {
   pcap <- plot_ly(res, x = ~pc$x[,1], y = ~pc$x[,2], z = ~pc$x[,3], color = ~cc, colors = c('#BF382A', '#0C4B8E'), width = NULL) %>%
    add_markers() %>% layout(scene = list(xaxis = list(title = "X"), yaxis = list(title = "Y"), zaxis = list(title = "Z")))}
   if(input$grad == TRUE) {
   pcap <- plot_ly(res, x = ~pc$x[,1], y = ~pc$x[,2], z = ~pc$x[,3], marker = list(color = ~cc, colorscale = c('#BF382A', '#0C4B8E'), showscale = TRUE), width = NULL) %>%
    add_markers() %>% layout(scene = list(xaxis = list(title = "X"), yaxis = list(title = "Y"), zaxis = list(title = "Z")))}
   }
  pcap$x$layout$width <- NULL
  pcap$x$layout$height <- NULL
  pcap$width <- NULL
  pcap$height <- NULL
  if(input$pca_checkbox == TRUE) {
  pcap}
  
 })
 
 #CLUSTERING#### 
 
 output$kmeans <- renderPlotly({
  if(is.null(df()))
   return(NULL)
  cCol <- cCol()
  axCol <- axCol()
  ayCol <- ayCol()
  dat <- df()
  dt <- unique(dat)
  lst <- lapply(dt[cCol], function(x) (x-min(x))/(max(x)-min(x)))
  
  df <- data.frame()
  
  res <- rbind(df, lst)
  
  
  kMeansResult = kmeans(res, centers = input$num, algorithm = input$alg)
  
  dd.col <- rainbow(length(as.numeric(levels(as.factor(kMeansResult$cluster)))))
  
  names(dd.col) <- levels(as.factor(kMeansResult$cluster))

  kmeansp <- ggplot(res, aes(x=dat[[axCol]], y =dat[[ayCol]], col = as.factor(kMeansResult$cluster))) + geom_point() + scale_color_brewer(palette="Dark2") +  
  labs(color = "Clusters", x = axCol, y = ayCol)
  kmeansp <- ggplotly(kmeansp)
  
  kmeansp$x$layout$width <- NULL
  kmeansp$x$layout$height <- NULL
  kmeansp$width <- NULL
  kmeansp$height <- NULL
  
  if(input$kmeans_checkbox == TRUE) {
   kmeansp}
  
  
 })
 
 output$hieclu <- renderPlotly({
  if(is.null(df()))
   return(NULL)
  cCol <- cCol()
  dat <- df()
  axCol <- axCol()
  ayCol <- ayCol()
  lst <- lapply(dat[cCol], function(x) (x-min(x))/(max(x)-min(x)))
  
  df <- data.frame(dat[[input$zcol]])
  
  res <- cbind(df, lst)
  
  clusters <- hclust(dist(res[,-1]), method = input$met)
  clusterCut <- cutree(clusters, k = input$num)
  
  hieclup <- ggplot(res, aes(x = dat[[axCol]], y = dat[[ayCol]], col = as.factor(clusterCut))) + geom_point() + scale_color_brewer(palette="Dark2") + labs(color = "Clusters", x = axCol, y = ayCol)
  hieclup <- ggplotly(hieclup)
  
  hieclup $x$layout$width <- NULL
  hieclup $x$layout$height <- NULL
  hieclup $width <- NULL
  hieclup $height <- NULL
  
  if(input$hieclu_checkbox == TRUE) {
   hieclup} 
  
 })
 
 output$sumclus <- DT::renderDataTable({
  if(is.null(df()))
   return(NULL)
  dat <- df()
  cCol <- cCol()
  lst <- lapply(dat[cCol], function(x) (x-min(x))/(max(x)-min(x)))
  df <- data.frame()
  res <- rbind(df, lst)
  fit1 <- kmeans(res, centers = input$num, algorithm = input$alg)
  df2 <- data.frame(dat[[input$zcol]])
  res2 <- cbind(df2, lst)
  d <- dist(res2, method = "euclidean")
  clusters <- hclust(dist(res2[,-1]), method = input$met)
  clusterCut <- cutree(clusters, k = input$num)
  l <- cluster.stats(d, fit1$cluster, clusterCut)
  fit.parameters.clus <- data.frame(Diameter=numeric(),
                                    Averaged=numeric(),
                                    Mediand=numeric(),
                                    Withcluss=numeric(),
                                    Entropy=numeric(),
                                    stringsAsFactors=FALSE)
  
  val.parameters<-data.frame(l$diameter, l$average.distance, l$median.distance, l$within.cluster.ss, l$entropy,stringsAsFactors=FALSE)
  colnames(val.parameters)<-c("Diameter", "Average Distance","Median Distance","Within Cluster SS","Entropy")
  fit.parameters.clus<-rbind(fit.parameters.clus, val.parameters)
  mySummary.clus <- fit.parameters.clus
  mySummary.clus[,] <- round(mySummary.clus[,], 4)
  if((input$kmeans_checkbox == TRUE) & (input$hieclu_checkbox == TRUE)) {
  try(DT::datatable(mySummary.clus, options = list(scrollX = TRUE)))}
 })
 
 output$down2 <- downloadHandler('Clustering-Summary.csv', 
                                content <- function(file) {
                                 if(is.null(df()))
                                  return(NULL)
                                 dat <- df()
                                 cCol <- cCol()
                                 lst <- lapply(dat[cCol], function(x) (x-min(x))/(max(x)-min(x)))
                                 df <- data.frame()
                                 res <- rbind(df, lst)
                                 fit1 <- kmeans(res, centers = input$num, algorithm = input$alg)
                                 df2 <- data.frame(dat[[input$zcol]])
                                 res2 <- cbind(df2, lst)
                                 d <- dist(res2, method = "euclidean")
                                 clusters <- hclust(dist(res2[,-1]), method = input$met)
                                 clusterCut <- cutree(clusters, k = input$num)
                                 l <- cluster.stats(d, fit1$cluster, clusterCut)
                                 fit.parameters.clus <- data.frame(Diameter=numeric(),
                                                                   Averaged=numeric(),
                                                                   Mediand=numeric(),
                                                                   Withcluss=numeric(),
                                                                   Entropy=numeric(),
                                                                   stringsAsFactors=FALSE)
                                 
                                 val.parameters<-data.frame(l$diameter, l$average.distance, l$median.distance, l$within.cluster.ss, l$entropy,stringsAsFactors=FALSE)
                                 colnames(val.parameters)<-c("Diameter", "Average Distance","Median Distance","Within Cluster SS","Entropy")
                                 fit.parameters.clus<-rbind(fit.parameters.clus, val.parameters)
                                 mySummary.clus <- fit.parameters.clus
                                 mySummary.clus[,] <- round(mySummary.clus[,], 4)
                                 
                                 
                                 write.csv(mySummary.clus, file, sep ='', row.names = FALSE)})
})#END