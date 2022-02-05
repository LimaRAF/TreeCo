#' @title Number of Unidentifications per Survey
#'
#' @description Calculate the number of individuals and species that are
#' not identified at different taxonomic levels (e.g. family, genus).
#'
#' @param tree.data the data frame with tree abundance data in the TreeCo format
#' @param group vector with the the column that should be used to aggregate the
#'   data. Default to 'ordem'.
#' @param spp.name the name of the columns containing the (morpho)species names.
#'   Default to the TreeCo column 'species.correct'.
#' @param tax.rank the name of the columns containing the name taxonomic resolution.
#'   Default to the TreeCo column 'taxon.rank'.
#' @param indets a vector of characters containing the unidentified classes in
#'   `tax.rank`. Default to the TreeCo classes 'unidentified', 'family' and
#'   'genus'.
#' @param not.indets a vector of characters containing the identified classes in
#'   `tax.rank`. Default to the TreeCo classes 'species', 'variety' and
#'   'subspecies'.
#' @param rm.flora should the records of (morpho)species not found within the
#'   main sampling but found in the floristics or natural regeneration of the
#'   same survey be included? Default to TRUE.
#' 
#' @import data.table
#' @importFrom utils tail
#' 
#' @return a data frame with number of individuals (N) and of taxa (S) for the each group and
#' the percentage of N and S unidentified at different taxonomic resolutions.
#'
#' @details The function returns the percentage of individuals and species that
#'   remained unidentified until a certain taxonomic rank, which is provided by
#'   the user. *The order of the vector provided in `indets` must be from the lowest
#'   to the highest resolution within the unidentification levels*.
#'   
#'   The taxonomic ranks include in TreeCo are, in descreasing order
#'   of taxonomic resolution: 'variety'/'subspecies', 'species', 'genus',
#'   'family', 'unidentified', 'dead' and 'correction'. The class 'unidentified'
#'   include living trees for which not even a family name was assigned. The
#'   class 'correction' is restricted to the Minas Gerais state inventory data.
#'   This class enter the computations of the total number of individuals in the
#'   sample but not in the totoal number of taxa.
#'   
#'   The proportions are cumulative and they respect the order of taxonomic
#'   levels provided by the user. By default, this means that proportions at
#'   genus level include the identifications at the genus, family and
#'   unidentified classes, proportions at family level include the
#'   identifications at family and unidentified resolutions. 
#'   
#'   Some TreeCo surveys contain extra species lists from outside the sampling
#'   plots/transects. These lists are often obtaining from floristic surveys in
#'   the same sites where the sampling was conducted. Other surveys stored in
#'   TreeCo present the records for species present in the adult and natural
#'   regeneration components of the forest. If a species is present in the
#'   floristic survey but not in the plots, the species enters the database with
#'   an abundance value equal to zero. Similarly, if a species is present in the
#'   natural regeneration but not in the adult component, the species enters the
#'   database with an abundance between zero and one. This records are excluded by
#'   default, suing the argument `rm.flora`.
#'   
#' @seealso \link[TreeCo]{traitCover}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export indetSpecies
#' 
indetSpecies <- function(tree.data = NULL, group = "ordem", spp.name = "species.correct", tax.rank = "taxon.rank", 
                     indets = c("unidentified","family","genus"), not.indets = c("species","variety","subspecies"),
                     rm.flora = TRUE) {

  #Escaping R CMD check notes from using data.table syntax
  tax.ranks <- group.by <- N <- S <- V1 <- NULL
  spp.names <- S.ranks <- N.ranks <- S.identified <- N.identified <- NULL
  
  ## Checking input
  if (is.null(tree.data))
    stop("Please provide the input species abundance data")
  
  ## Removing species not found in the main tree sampling (natural regeneration of floristics) - Is it necessary for trait analysis?
  if (rm.flora)
    tree.data <- tree.data[tree.data$N >= 1,]
  
  ## Defining the column with the grouping variable(s) and taxon ranks
  tree.data$group.by <- tree.data[, group]
  tree.data$tax.ranks <- tree.data[, tax.rank]
  tree.data$spp.names <- tree.data[, spp.name]
  DT <- data.table::as.data.table(tree.data)

  ## Getting the vector of ranks
  ranks <- c(indets, not.indets)
  no.ranks <- unique(DT$tax.ranks)[!unique(DT$tax.ranks) %in% ranks]
  ranks <- ranks[ranks %in% unique(DT$tax.ranks)]
  if (length(no.ranks) > 0)
    warning(paste0("The unspecified taxon rank(s) ", paste0(no.ranks, collapse = " + "),
                   " was(were) were removed from the data prior to calculations"))
  indets <- indets[indets %in% ranks] 
  DT <- DT[tax.ranks %in% ranks, ]

  ## Obtaining the total number of records, individuals and species per group
  data.table::setkey(DT, group.by)
  indets.DT <- DT[ , .N, by = group.by] # Records
  data.table::setnames(indets.DT, c("N"), c("records"))
  indets.DT[, N := DT[ , sum(N), by = group.by]$V1,] # Number of individuals
  indets.DT[, S := DT[ , data.table::uniqueN(spp.names), by = group.by]$V1,] # Number of species

  ## Obtaining the number of individuals and species per taxon rank per group
  data.table::setkey(DT, group.by, tax.ranks)
  tmp.DT <- DT[data.table::CJ(group.by, tax.ranks, unique = TRUE), sum(N), by = .EACHI]
  tmp.DT[is.na(V1), V1 := 0]
  data.table::setnames(tmp.DT, c("V1"), c("N.ranks"))
  tmp.DT[, S.ranks := DT[data.table::CJ(group.by, tax.ranks, unique = TRUE), 
                          data.table::uniqueN(spp.names), by = .EACHI]$V1, ]
  tmp.DT[N.ranks == 0 & S.ranks == 1, S.ranks := 0]
  
  ## Calculating the number of unidentifications until a given taxon.rank
  for (i in 1:length(indets)) {
    tmp <- tmp.DT[tax.ranks %in% indets[1:i],]
    data.table::setkey(tmp, group.by)
    tmp1 <- tmp[ , lapply(.SD, sum), by = group.by, .SDcols = c("N.ranks", "S.ranks") ]
    nomes <- paste(c("N.", "S."), indets[i], sep = "")
    data.table::setnames(tmp1, c("N.ranks", "S.ranks"), c(nomes))
    indets.DT <- data.table::merge.data.table(indets.DT, tmp1, by="group.by", all = TRUE, sort = FALSE)
  }
  
  ## Calculating the number of complete identifications
  col.names.N <- names(indets.DT)[grepl("N\\.", names(indets.DT))]
  col.names.S <- names(indets.DT)[grepl("S\\.", names(indets.DT))]
  indets.DT[ , N.identified := N - .SD, .SDcols = c(utils::tail(col.names.N, 1))]
  indets.DT[ , S.identified := S - .SD, .SDcols = c(utils::tail(col.names.S, 1))]
  
  ## Getting the percentages
  col.names.N <- names(indets.DT)[grepl("N\\.", names(indets.DT))]
  indets.DT[ , c(col.names.N) := lapply(.SD, function(x) round(100 * x/N, 4)),
             .SDcols = c(col.names.N)]
  col.names.S <- names(indets.DT)[grepl("S\\.", names(indets.DT))]
  indets.DT[ , c(col.names.S) := lapply(.SD, function(x) round(100 * x/S, 4)),
             .SDcols = c(col.names.S)]

  ## Preparing to return
  data.table::setnames(indets.DT, c("group.by"), c(group))
  return(as.data.frame(indets.DT))
}
