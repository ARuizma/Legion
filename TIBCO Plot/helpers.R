
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

