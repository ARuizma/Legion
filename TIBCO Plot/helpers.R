####PLOT#####
.addXaxis <- function(x, showAsLog){
 x <- unique(x)
 x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), len = length(x))
 if(!showAsLog){
  l <- 10^seq(-20, 20)
  axis(1, at = log10(l), labels = format(l, digits = 1, scientific = TRUE), cex.axis = .85)
 } else{
  axis(1, at = x, labels = format(x, digits = 2, scientific = FALSE), cex.axis = .85)
 }
}
.addYaxis <- function(y){
 y <- seq(-1, 2, by = .25)
 axis(2, at = y, labels = format(y, digits = 2), cex.axis = .85, las = 1)
}



.multiCurve <- function(df, showPoints = FALSE, showMeans = FALSE, showSDerr = TRUE, pSize = 1, lWidth=1, legendSize=1, showAsLog = TRUE, Legend = TRUE, Cols = NULL,...){
 
 
 showAsLog <- ifelse(showAsLog == "TRUE", TRUE, FALSE)
 K <- length(df)
 
 allX <- do.call(c, df) 
 allY <- do.call(c, df) 
 plot(range(allX, na.rm = TRUE), range(min(allY, na.rm = TRUE), max(allY, na.rm = TRUE)+.3),
      type = "n", bty = "n", axes = FALSE, cex.axis = .95,
      ylim = range(min(min(allY, na.rm = TRUE), 0), max(max(allY, na.rm = TRUE), 1.25)+.2), ...)
 
 .addXaxis(allX, showAsLog)
 .addYaxis(allY)
 
 if(is.null(Cols))
  Cols <- rep("black", K)
 
 for(k in seq_len(K)){
  tmp <- df[[k]]
  Col <- Cols[k]
  .addCurve(tmp, Col, lWidth)
 }
 
 if(min(allY, na.rm = TRUE) < 0)
  abline(h = 0, lty = 2)
 if(max(allY, na.rm = TRUE) > 1)
  abline(h = 1, lty = 2)
 
 if(Legend){
  nm <- length(names(df))
  nc <- sum(nchar(names(df)))
  #        Cex <- 1 - min(nc/90, .7)
  nc <- nc + 10*nm
  Cex <- 1.5*legendSize
  if(nc > 100){
   K <- ceiling(K/2)            
  }
  legend("top", legend = names(df), ncol = K,
         col = Cols, bty = "n", cex = Cex, lwd = 2, pch = 19)
 }
}