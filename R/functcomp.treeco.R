#' @title Calculate Trait Community Weighted Means
#'
#' @description This function is adapted from the function `functcomp()` from
#'   package `FD` (Laliberté et al. 2014). It returns for each trait the
#'   community-level weighted means (CWM), which can be used to describe the
#'   functional composition of communities.
#'
#' @param x matrix or data frame containing the functional traits.
#' @param a matrix or data frame containing the abundances of the taxa listed in x.
#' @param CWM.type character string indicating how nominal, binary and ordinal
#'   traits should be handled. Default to "all".
#' @param bin.num vector indicating binary traits to be treated as continuous.
#' @param CWM.sd should the weighted standard deviation of the numerical traits de
#'   returned? Default to TRUE.
#' 
#' @importFrom SDMTools wt.sd
#' @importFrom stats weighted.mean
#' 
#' @details This function uses most of the codes from `FD::functcomp()`, so the
#'   help and details of this function should be consulted.
#'   
#'   The main differences of `functcomp.treeco()` from `FD::functcomp()` are:
#'    -- FINALIZAR --
#' @seealso \link[FD]{functcomp}   
#'
#' @references 
#' 
#'  Laliberté, E., Legendre, P., and B. Shipley. (2014). FD: measuring
#'  functional diversity from multiple traits, and other tools for functional
#'  ecology. R package version 1.0-12.
#'
#' @export functcomp.treeco
#' 
functcomp.treeco <- function (x, a, CWM.type = "all", bin.num = NULL, CWM.sd = TRUE) {

  ## Checking input data
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("'x' must be a matrix or a data frame.", "\n")
  } else { x <- data.frame(x) }
  
  if (!is.matrix(a)) 
    a <- as.matrix(a)
  
  if (is.null(row.names(x))) { 
    stop("'x' must have row names.", "\n")
  } else { x.n <- row.names(x) }
  
  if (is.null(colnames(a))) {
    stop("'a' must have column names.", "\n")
  } else { a.n <- colnames(a)}
  
  s.x <- dim(x)[1]
  s.a <- dim(a)[2]
  
  if (s.x != s.a) 
    stop("Different number of species in 'x' and 'a'.", "\n")
  if (any(x.n != a.n)) 
    stop("Species labels in 'x' and 'a' need to be identical and ordered alphabetically (or simply in the same order).", "\n")
  
  ## General description of input data
  com <- dim(a)[1]
  t <- dim(x)[2]
  com.names <- row.names(a)
  sp.names <- row.names(x)
  tr.names <- names(x)
  a[which(is.na(a))] <- 0
  
  #CWM.type <- match.arg(CWM.type)
  
  ## Defining the types of trait data
  type <- sapply(x, data.class)
  #binary
  is.bin <- function(k) all(k[!is.na(k)] %in% c(0, 1))
  bin.var <- rep(NA, t)
  names(bin.var) <- tr.names
  for (i in 1:t) bin.var[i] <- is.bin(x[, i])
  if (!all(bin.var[bin.num])) 
    stop("'bin.num' points to non-binary variables.\n")
  bin.var[bin.num] <- FALSE

  #numerical (continuous or discrete)  
  type[type %in% c("numeric", "integer")] <- "C"
  num.as.char <- suppressWarnings(
    sapply(x, function(y) sum(as.numeric(y), na.rm = TRUE) >=1))
  type[!type %in% "C" & num.as.char] <- "C"
  
  #ordered
  type[type == "ordered"] <- "O"
  #factor
  type[type == "factor"] <- "N"
  #correcting numerical to binary
  type[bin.var] <- "B"
  
  ## Transforming abundance to relative abundances  
  sum.a <- apply(a, 1, sum)
  a <- a/sum.a
  a <- data.frame(t(a))
  
  ## Getting the CWM for each trait x community combination
  temp <- list()
  temp.sd <- list()
  for (i in 1:t) {
    if (type[i] == "C") { # CWM for numerical traits
      vec <- numeric(com)
      vec.sd <- numeric(com)
      
      for (j in 1:com) {
        vec[j] <- stats::weighted.mean(x[, i], a[, j], na.rm = TRUE)
        na.ids <- !is.na(x[, i])
        vec.sd[j] <- SDMTools::wt.sd(x[na.ids, i], a[na.ids, j])
      }  
      temp[[i]] <- matrix(vec, com, 1, dimnames = list(com.names, tr.names[i]))
      temp.sd[[i]] <- matrix(vec.sd, com, 1, dimnames = list(com.names, tr.names[i]))
      
    } else {    # CWM for nominal, binary and ordinal traits
      x[, i] <- as.factor(x[, i])
      fac <- data.frame()
      # which.dom <- rep(NA, com)
      for (k in 1:com) {
        temp2 <- tapply(a[, k], x[, i], sum)
        fac <- rbind(fac, temp2)
        # which.dom[k] <- sample(levels(x[, i])[which(fac[k,] == max(fac[k, ]))], size = 1)
      }
      colnames(fac) <- paste(tr.names[i], "_", levels(x[,i]), sep = "")
      rownames(fac) <- com.names
      # which.dom <- data.frame(which.dom)
      # colnames(which.dom) <- tr.names[i]
      # rownames(which.dom) <- com.names
      # if (CWM.type == "dom") temp[[i]] <- which.dom
      # if (CWM.type == "all") temp[[i]] <- fac
      temp[[i]] <- fac
    }
  }
  temp <- data.frame(temp)
  
  ## Should the standard deviation of numerical traits be returned?
  if (CWM.sd) {
    temp.sd[sapply(temp.sd, is.null)] <- NULL
    temp.sd <- data.frame(temp.sd)
    names(temp.sd) <- paste0(names(temp.sd),".sd")
    temp <- cbind.data.frame(temp, temp.sd, stringsAsFactors = FALSE)
  }
  return(temp)
}
