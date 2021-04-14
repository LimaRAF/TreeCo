#' @title Prepare List of Species Names for Query
#'
#' @description From a list of names already cleaned and validated, the function
#' prepare all fields necessary to obtain species trait data from the TreeCo
#' database.
#'
#' @param x a data frame with, at least, the cleaned and validated taxa names
#' @param tax.name name of the column containing the cleaned and validate taxa
#'   names. Default to 'suggestedName'
#' @param spp.name name of the column containing the original species name, used
#'   to obtain the cleaned and validated names. Default to 'species.correct'.
#' @param fam.name name of the column containing the taxon family. Default to
#'   'family'.
#' @param genus.name name of the column containing the taxon genus. Default to
#'   'genus'.
#' @param tax.rank name of the column containing the taxon rank. Default to
#'   'taxon.rank'.
#' @param tax.status name of the column containing the taxon status. Default to
#'   'scientificNameStatus'.
#' @param out.names vector of column names in the output table for 'tax.name',
#'   'fam.name', 'genus.name', 'spp.name' and 'tax.rank', in this specific
#'   order.
#' 
#' @importFrom flora get.taxa
#' 
#' @return a data frame with ...
#'
#' @details The function ...
#'   
#' @seealso \link[TreeCo]{traitCover}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export speciesList
#' 
speciesList <- function(x = NULL, 
                        tax.name = "suggestedName",
                        spp.name = "species.correct",
                        fam.name = "family",
                        genus.name = "genus",
                        tax.rank = "taxon.rank",
                        tax.status = "scientificNameStatus",
                        out.names = c("Name_submitted","family","genus",
                                      "species.correct","taxon.rank")
) {
  
  ## Checking input
  if (is.null(x))
    stop("Please provide the data frame with the taxon name")

  if (!class(x)[1] == "data.frame")
    stop("Input data needs to be a data frame")
  
  if (!tax.name %in% colnames(x))
    stop("Please a data frame with the correct taxon name or redefine it")

  ## Filtering the input data
  x <- x[, colnames(x) %in% c(tax.name, spp.name, fam.name,
                                genus.name, tax.rank, tax.status)]
  
  ## If the necessary columns are no present, create them
  if (!spp.name %in% colnames(x)) 
    x[, spp.name] <- x[, tax.name] 

  if (!genus.name %in% colnames(x)) {
    x[, genus.name] <- 
      sapply(x[ ,tax.name], function(x) strsplit(x," ", fixed = TRUE)[[1]][1])
  } else { 
    check_these <- is.na(x[, genus.name])
    if (any(check_these))
    x[, genus.name][check_these] <- 
      sapply(x[ ,tax.name][check_these], function(x) strsplit(x," ", fixed = TRUE)[[1]][1])
  }
  
  ## Getting family and taxon.rank per species and genus from Flora do Brasil
  x$tmp.order <- 1:dim(x)[1]
  tmp.spp <- flora::get.taxa(unique(x[, tax.name]), drop = c(""))
  cols <- c("original.search","family","taxon.rank","notes")
  combo <- merge(x, tmp.spp[ ,cols], by.x = tax.name, by.y = "original.search",
                 all.x = TRUE, sort = FALSE)
  tmp.genus <- flora::get.taxa(unique(x[, genus.name]))
  combo1 <- merge(x, tmp.genus[ ,cols], by.x = genus.name, by.y = "original.search",
                 all.x = TRUE, sort = FALSE)
  check_these <- is.na(combo$family) & !is.na(combo1$family)
  combo[check_these , cols[-1]] <- combo1[check_these , cols[-1]]
  combo <- combo[order(combo$tmp.order), ]
    
  if (!fam.name %in% colnames(x)) {
    x[ ,fam.name] <- combo$family
  } else { 
    check_these <- is.na(x$family)
    if (any(check_these))
      x[, fam.name][check_these] <- 
        combo$family[check_these]
  }
  
  if (!tax.rank %in% colnames(x)) {
    x[ ,tax.rank] <- combo$taxon.rank
    
    check_these <- is.na(x[ ,tax.rank])
    if (any(check_these) & tax.status %in% colnames(x))
      x[, tax.rank][check_these] <- 
        x[, tax.status][check_these]
    
    
  } else { 
    check_these <- is.na(x[ ,tax.rank])
    if (any(check_these))
      x[, tax.rank][check_these] <- 
        combo$taxon.rank[check_these]
    
    check_these1 <- is.na(x[ ,tax.rank])
    if (any(check_these1) & tax.status %in% colnames(x))
      x[, tax.rank][check_these1] <- 
        x[, tax.status][check_these1]
    
  }
  
  ##Homogenizing the taxon rank classes
  x[, tax.rank] <- gsub("possibly_ok", "species", x[, tax.rank], fixed = TRUE) 
  x[, tax.rank] <- gsub("indet", "unidentified", x[, tax.rank], fixed = TRUE) 
  x[, tax.rank] <- gsub("subfamily_as_genus", "family", x[, tax.rank], fixed = TRUE) 
  
  ##Preparing to return
  x1 <- x[, c(tax.name, fam.name, genus.name, spp.name, tax.rank)]
  colnames(x1) <- out.names
  
  return(x1)
}
