# R-squared
rsq <- function(y, y_hat) {
  sum((y_hat - mean(y))^2) / sum((y - mean(y))^2)
}