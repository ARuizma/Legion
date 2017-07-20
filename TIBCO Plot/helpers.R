pki.app.s4s.CurveFitting.Fit.Logistic <- function(DFunction, start.list, a = NA, b = NA, c = NA, d = NA) {
 ModelName <- "Logistic Regression"
 Fitresult <- "OK"
 fit <- NULL
 
 iter = 0
 while (is.null(fit) & is.na(d) & (iter < 3)) {
  try(
   fit <- nlsLM(y ~ a + ((b - a) / (1 + ((x / c) ^ d))), DFunction,
              start = start.list,
              control = list(warnOnly = TRUE, minFactor = 1 / 1024), na.action = na.omit))
  if (start.list[["d"]] > 0) {
   start.list["d"] = 2 * ceiling(start.list[["d"]])
  } else {
   start.list["d"] = 2 * floor(start.list[["d"]])
  }
  iter<- iter + 1
 }
 
 GoF <- NULL
 if (!is.null(fit)) {
  # If no error, return the values)
  GoF <- pki.app.s4s.CurveFitting.GoF(DFunction, fit)
  if (is.na(a)) {
   a = coef(fit)[["a"]]
  }
  if (is.na(b)) {
   b = coef(fit)[["b"]]
  }
  c = coef(fit)[["c"]]
  if (is.na(d)) {
   d = coef(fit)[["d"]]
  }
 } else {
  Fitresult <- "Warning: Logistic Regression Fit did not converge. Using parameter estimates"
  GoF$SSE <- 1e+100
  GoF$TSS <- 1e+100
  GoF$RSquared = 0
  GoF$AdjustedRSquared = 0
  GoF$DegreesOfFreedom = 0
  GoF$RMSE = 1e+100
  if (is.na(a)) {
   a = start[["a"]]
  }
  if (is.na(b)) {
   b = start[["b"]]
  }
  c = start[["c"]]
  if (is.na(d)) {
   d = start[["d"]]
  }
 }
 mylist <- list("fit" = fit, "ModelName" = ModelName,
                "A" = a, "B" = b, "C" = c, "D" = d,
                "SSE" = GoF$SSE, "TSS" = GoF$TSS,
                "DegreesOfFreedom" = GoF$DegreesOfFreedom,
                "RSquared" = GoF$RSquared,
                "AdjustedRSquared" = GoF$AdjustedRSquared,
                "RMSE" = GoF$RMSE, "FitResult" = Fitresult)
 return(mylist)
}

# Goodness of fit calculations
pki.app.s4s.CurveFitting.GoF <- function(DFunction, fit,a,b,d) {
 # Residual sum of squares (RSS), 
 # Also known as the sum of squared residuals (SSR) 
 # or the sum of squared errors of prediction (SSE) 
 # It is the sum of the squares of residuals 
 # (deviations of predicted from actual empirical values of data).
 SSE <- sum(residuals(fit)^2)
 
 # Total sum of squares
 TSS <- sum((DFunction$y - mean(DFunction$y))^2)
 
 #Degrees of freedom
 DegreesOfFreedom <- df.residual(fit)
 
 # R squared
 if (TSS != 0) { 
  RSquared = 1 - (SSE/TSS) 
 } else {
  RSquared = 1 
 }	
 
 # Adjusted R squared
 n <- nrow(DFunction)
 if ((TSS*DegreesOfFreedom) != 0) { 
  AdjustedRSquared = 1 - ((SSE*(n-1))/(TSS*DegreesOfFreedom)) 
 } else {
  AdjustedRSquared = 1
 }
 
 # Root mean Square Error
 if (DegreesOfFreedom != 0) { 
  RMSE = sqrt(SSE/DegreesOfFreedom)
 } else {
  RMSE = 1e100
 }
 
 mylist <- list("SSE" = SSE, "TSS" = TSS, "DegreesOfFreedom" = DegreesOfFreedom, "RSquared" = RSquared, "AdjustedRSquared" = AdjustedRSquared, "RMSE" = RMSE)
 return(mylist)
}

pki.app.s4s.get.starting.parameters <- function(input.data, fixed = list()) {
 # Set starting parameters
 if(is.null(input.data))
  return(NULL)
 x <- input.data[, "x"]
 y <- input.data[, "y"]
 n <- nrow(input.data)
 xy <- x * y
 
 start <- list()
 
 if (!"a" %in% names(fixed)) {
  start["a"] = quantile(y, 1, na.rm = TRUE)[[1]]
 }
 
 if (!"b" %in% names(fixed)) {
  start["b"] = quantile(y, 0, na.rm = TRUE)[[1]]
 }
 
 if (length(y) > 2) {
  mid.y <- (max(y) + min(y)) / 2
  start["c"] = input.data[which.min(abs(input.data$y - mid.y)), "x"]
 } else {
  start["c"] = mean(x, na.rm = TRUE)[[1]]
 }
 
 d = 0
 if (!"d" %in% names(fixed)) {
  d <- (n * (sum(xy) - sum(x) * sum(y))) / abs(n * (sum(x ^ 2) - sum(y ^ 2)))
 }
 if (is.na(d)) {
  start["d"] <- 0
 } else if (d < 1) {
  start["d"] = floor(d)
 } else {
  start["d"] = ceiling(d)
 }
 
 return(start)
}


