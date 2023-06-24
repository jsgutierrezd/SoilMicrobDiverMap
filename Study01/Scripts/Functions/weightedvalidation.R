

# Weighted RMSE
RMSEw <- function(d, w)
{
  sqe <- w*(d[, 1] - d[, 2])^2
  msqe <- sum(sqe)/sum(w)
  out <- sqrt(msqe)
  return(out)
}


# Weighted R^2
R2w <- function(d, w)
{
  require(boot)
  out <- boot::corr(d[, 1:2], w)^2
  return(out)
}


# Weighted summary function

WeightedSummary <- function(data, lev = NULL, model = NULL, ...)
{
  out <- numeric()
  # Weighted RMSE
  RMSEw <- function(d, w)
  {
    sqe <- w*(d[, 1] - d[, 2])^2
    msqe <- sum(sqe)/sum(w)
    out <- sqrt(msqe)
    return(out)
  }
  out[1] <- RMSEw(data[, 1:2], data$weights)
  
  # Weighted R^2
  require(boot)
  out[2] <- boot::corr(data[, 1:2], data$weights)^2
  names(out) <- c('RMSEw', 'R2w')
  out
}