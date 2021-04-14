#' @title Weighted Mean, Variance and Standard Deviation
#'
#' @description Calculates the weighted mean and the variance or standard
#'   deviation for a given weighted mean.
#'
#' @param x numerical. A vector of values to compute the weighted statistics.
#' @param w numerical. A vector of weights with the same length as \code{x}.
#' @param na.rm logical. Should \code{NA} values be removed? Default to FALSE.
#' @param stats character. Which weighted statistic should be returned: "mean",
#'  "var" or "sd"? 
#'
#' @details Weigths should vary between 0 and 1. If they don't, they are
#'   normalized to sum one. Weights equal to 0 means that the corresponding
#'   values in \code{x} will be omitted from the from the calculation.
#'
#' @author Renato A. F. de Lima
#'
#' @keywords internal
#'
#' @examples
#' 
#' x <- c(1,3,4,8)
#' w <- c(0.2,0.5,0.7,0.9)
#' mean(x); weighted.stats(x, w)
#' sd(x); weighted.stats(x, w, stats = "sd")
#'
#' @export
weighted.stats <- function(x, w, na.rm = FALSE, stats = "mean") {
  
  if (length(x) != length(w))
    stop("'x' and 'w' must have the same length")

  if (na.rm) {
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  
  if (any(w < 0 | w >1 )) {
    if (any(w < 0)) {
      rg <- range(w)
      w1 <- (rg[2] - rg[1]) * ((w - rg[1]) / (rg[2] - rg[1])) - rg[1]
      w <- w1 / sqrt(sum(w1 ^ 2))
    } else {
      w <- w / sqrt(sum(w ^ 2))
    }
  }

  w.mean <- sum(w * x)/sum(w)
  w.var <- (sum(w * x * x)/sum(w)) - (w.mean ^ 2) # package Weighted.Desc.Stat
  # w.var <- sum(w * (x - w.mean) ^ 2)*(sum(w)/(sum(w) ^ 2 - sum(w ^ 2))) # package SDMTools
  # w.var <- (sum(w * (x - w.mean) ^ 2)) / sum(w) # Wikipedia
  
  if (stats == "mean")
    return(w.mean)
  if (stats == "var")
    return(w.var)
  if (stats == "sd") {
    w.sd <- sqrt(w.var) 
    return(w.sd)
  }
} 
