#' @title Get Author Names
#'
#' @description Function to extract and tabulate the names of authors 
#' based on a list of reference IDs and the TreeCo standard reference 
#' table
#'  
#' @param ref.ids a vector with the list of TreeCo refIDs
#' @param ref.data the data frame with TreeCo reference database
#' @param ids.coluna the name of the column in 'ref.data' containing the
#'   TreeCo refIDs. Defaults to 'refID'.
#' @param auth.coluna the name of the column in 'ref.data' containing the
#'   author names. Defaults to 'author'.
#' @param adv.coluna the name of the column in 'ref.data' containing
#'   the advisor name in the case of thesis and dissertations.
#'   Defaults to 'advisor'.
#' @param fuzzy logical. Should authors with different but close
#'   spelling be put together (may take time for large lists of
#'   authors). Defaults to FALSE.
#' @param max.dist numerical. The maximum distance between strings to
#'   put authors names together.
#' 
#' @details If the argument 'ref.IDs' is empty, all references in
#'   'ref.data' will be used to get the count of author names
#'   
#'   Distances between authors names are performed using the
#'   `stringdistmatrix()` function from package stringdist and using
#'   the method 'jw' (i.e. Jaro-Winkler) for for distance calculation.
#'   For large selections of references, this may take a while.
#'   
#' @author Renato A. F. de Lima
#' 
#' @importFrom plantR prepName
#' @importFrom stringdist stringdistmatrix
#' @importFrom stats aggregate cutree hclust
#'
#' @export refAuthors
#' 
refAuthors <- function(ref.ids = NULL,
                    ref.data = NULL, 
                    ids.coluna = "refID",
                    auth.coluna = "author",
                    adv.coluna = "advisor",
                    fuzzy = FALSE,
                    max.dist = 0.025) {
  
  if (is.null(ref.data))
    stop("Please provide the input reference")
  
  if (!"data.frame" %in% class(ref.data))
    stop("Please provide a data frame as an input")
  
  ref.data <- as.data.frame(ref.data)
  
  if (!any(ids.coluna %in% names(ref.data)))
    stop("Please provide a reference ID column name matching the input data frame")

  if (!any(auth.coluna %in% names(ref.data)))
    stop("Please provide an author column name matching the input data frame")
  
  if (!is.null(ref.ids)) {
    ref.data <- ref.data[ref.data[[ids.coluna]] %in% ref.ids, ]
    if (dim(ref.data)[1] == 0)    
      stop("No reference ID matching the list in 'ref.ids' was found")
  }
  
  rm_these <- ref.data[[ids.coluna]] %in% c("", " ", NA, "NA")
  if (any(rm_these)) {
    ref.data <- ref.data[!rm_these, ] 
    warning("One or more references without IDs were removed from 'ref.data'", 
            call. = FALSE)
  }
  
  if (any(adv.coluna %in% names(ref.data))) {
    add_these <- !is.na(ref.data[[adv.coluna]])
    if (any(add_these))
      ref.data[[auth.coluna]][add_these] <- 
        paste(ref.data[[auth.coluna]][add_these], 
              ref.data[[adv.coluna]][add_these], sep="; ")
  }
  
  autores <- ref.data[[auth.coluna]]
  refIDs <- ref.data[[ids.coluna]]
  
  autores1 <- plantR::prepName(autores, sep.out = "; ", special.char = FALSE,
                              format = "last_init", pretty = TRUE,
                              get.prep = FALSE, get.initials = TRUE)
  autores2 <- strsplit(autores1, "; ")
  
  ## any author position
  nome.col <- "count"
  result <- data.frame(unlist(autores2),
                       rep(refIDs, times = lengths(autores2)))
  names(result) <- c(auth.coluna, ids.coluna)
  
  result1 <- stats::aggregate(as.character(result[[ids.coluna]]), 
                       list(result[[auth.coluna]]), paste0, 
                       collapse = "|")
  names(result1)[1] <- auth.coluna
  result1[[nome.col]] <- 
    # lengths(regmatches(result1$x, gregexpr("\\|", result1$x))) + 1 
    nchar(as.character(result1$x)) - nchar(gsub("|", "", result1$x, 
                                                fixed = TRUE)) + 1

  ## first or last author
  nome.col1 <- "count_1st_last"
  autores3 <- lapply(autores2, function(x) x[c(1, length(x))])
  autores3 <- lapply(autores3, function(x) unique(x))
  
  result <- data.frame(unlist(autores3),
                       rep(refIDs, times = lengths(autores3))
  )
  names(result) <- c(auth.coluna, ids.coluna)
  
  result2 <- stats::aggregate(as.character(result[[ids.coluna]]), 
                       list(result[[auth.coluna]]), paste0, 
                       collapse = "|")
  names(result2)[1] <- auth.coluna
  result2[[nome.col1]] <- 
    # lengths(regmatches(result1$x, gregexpr("\\|", result1$x))) + 1 
    nchar(as.character(result2$x)) - nchar(gsub("|", "", result2$x, 
                                                fixed = TRUE)) + 1
  
  result3 <- merge(result1, result2, by = auth.coluna, all = TRUE)
  result3 <- result3[order(result3[[nome.col]], decreasing = TRUE), 
                     c(auth.coluna, nome.col, nome.col1)]  
  
  ## Finding very similar author names and merging
  if (fuzzy == TRUE) {
    hc <- stats::hclust(stringdist::stringdistmatrix(
      plantR:::cleanName(result3[[auth.coluna]]), 
      method = "jw", 
      useNames = "strings"))
    df2 <- data.frame(c(result3[[auth.coluna]]), 
                      stats::cutree(hc, h = max.dist))
    names(df2) <- c(auth.coluna, "cluster")
    result4 <- merge(df2, result3, by = auth.coluna, all.x = TRUE)
    tmp0 <- stats::aggregate(result4[[nome.col]], list(result4$cluster),
                      sum, na.rm = TRUE)
    tmp1 <- stats::aggregate(result4[[nome.col1]], list(result4$cluster),
                      sum, na.rm = TRUE)
    result5 <- merge(tmp0, tmp1, by = "Group.1", all = TRUE)
    names(result5) <- c("cluster", nome.col, nome.col1)
    tmp2 <- aggregate(result4[[auth.coluna]], list(result4$cluster),
                paste0, collapse = "|")
    names(tmp2) <- c("cluster", auth.coluna)
    result.final <- merge(tmp2, result5, by = "cluster", all.x = TRUE)
    result.final <- result.final[, c(auth.coluna, nome.col, nome.col1)]
    result.final <- result.final[order(result.final[[nome.col]], 
                                       decreasing = TRUE), ]
    return(result.final)
  } else {
    result.final <- result3
    row.names(result.final) <- NULL
    result.final[is.na(result.final)] <- 0
    return(result.final)
  }
}
