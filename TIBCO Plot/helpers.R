# Library
pki.app.s4s.CurveFitting.Fit.Logistic <- function(DFunction, start.list, a = NA, b = NA, c = NA, d = NA) {
 ModelName <- "Logistic Regression"
 Fitresult <- "OK"
 fit <- NULL
 
 iter = 0
 while (is.null(fit) & is.na(d) & (iter < 3)) {
  try(
   fit <- nls(y ~ a + ((b - a) / (1 + ((x / c) ^ d))), DFunction,
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


# Start of the datafunction

# Check for required input parameters
if (!'measurement.data' %in% ls()) {
 stop("No input table provided")
}
if (!'concentration' %in% ls()) {
 stop("No concentration column provided")
}


# Check for required input columns
# Check that the input table has compound, concentration and Exclusion columns
# Get a list of all the compounds in the table
all.compounds<-list()
if (!compound %in% colnames(measurement.data)) {
 stop("No Compound column in selected table")
} else {
 all.compounds<-unique(measurement.data[,compound])
 all.compounds<-all.compounds[!is.na(all.compounds)]
}

if (!concentration %in% colnames(measurement.data)) {
 stop("No Concentration column in selected table")
}

# Check for required properties
if (!'selected.compound' %in% ls()) {
 stop("No compound ID provided")
} else {
 # Select the first compound
 selected.compound <- unlist(strsplit(selected.compound,split=";"))
 # The reason for this is to avoid trying to fit a nonexistet compound if the splitting is incorrect due to
 # special characters
 selected.compound<-intersect(selected.compound,all.compounds)
 selected.compound <- selected.compound[1]
}

# If there is no selected compound, select the first
if (is.na(selected.compound)) {
 selected.compound<-all.compounds[1]
}

if (!'selected.feature' %in% ls()) {
 stop("No feature ID provided")
}

# Get the parameters for the calculation
selection <- measurement.data[, compound] == selected.compound

# If an exclusion column exists use only included rows
if ('pki.app.s4s.curvefitting.exclusion.tag' %in% colnames(measurement.data)) {
 included<-measurement.data[,"pki.app.s4s.curvefitting.exclusion.tag"]!="Excluded"
 selection <- selection & included
} else {
 print("No exclusion column found. Using all entries")
}

# Get the entries for the feature and compund of interest
input.data<-data.frame(measurement.data[selection,c(compound,concentration)],
                       measurement.data[selection,selected.feature])
colnames(input.data)<-c("Compound","x","y")

# Remove NAs
input.data<-input.data[!is.na(input.data$x),]

# Get the mean for each concentration
if (length(input.data[,1] > 0)) {
 input.data<-aggregate(y~Compound + x,input.data,mean)
}

input.data<-input.data[complete.cases(input.data),]
# Remove cases where x < 0 to avoid problems with the log
input.data<-input.data[input.data[,"x"] > 0,]

DFunction=unique(input.data)

# Set starting parameters
# Set Max 
if ('max' %in% ls() && fix.a) {
 a.start = max
} else {
 a.start=NA
}

# Set Min
if ('min' %in% ls() && fix.b) {
 b.start = min
} else {
 b.start=NA
}

# Set slope
if ('slope' %in% ls() && fix.d) {
 d.start = slope
} else {
 d.start=NA
}

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

for (val in selected.compound) {
 comp.data<-DFunction[DFunction[,"Compound"]==val,]
 fit<-NULL
 if (dim(comp.data)[1] > 3) {
  start<-pki.app.s4s.get.starting.parameters(comp.data)
  
  # Fix the values for those starting parameters that are to be constrained 
  if (!is.na(a.start)) {
   start[["a"]]<-NULL
  }
  if (!is.na(b.start)) {
   start[["b"]]<-NULL
  }
  if (!is.na(d.start)) {
   start[["d"]]<-NULL
  }
  fit<-pki.app.s4s.CurveFitting.Fit.Logistic(comp.data,start,a=a.start,b=b.start,d=d.start)
  
  #Generate the final result
  val.parameters<-data.frame(val,selected.feature,
                             fit$B,fit$A,log10(fit$C),fit$D,fit$RSquared,fit$C,
                             fit$FitResult)
 } else {
  val.parameters<-data.frame(val,selected.feature,
                             0,0,0,0,0,0,
                             "Insufficient data to fit")
  fit$RSquared<-0
  fit$A<-0
  fit$B<-0
  fit$D<-0
 }
 colnames(val.parameters)<-c(compound,"Feature","min","max","LoggedX50","Hill","r2","Inflexion","Notes")
 fit.parameters<-rbind(fit.parameters, val.parameters)
 
 # Generate the final result
 r2<-fit$RSquared
 newMin<-fit$B
 newMax<-fit$A
 newSlope<-fit$D
}
pki.app.s4s.fit.parameters<-fit.parameters