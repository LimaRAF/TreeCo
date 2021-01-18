#' @title Assess Trait Species Coverage
#'
#' @description Calculate the number of individuals and taxa per survey that
#' have any information available for selected species traits.
#'
#' @param tree.data the data frame with tree abundance data in the TreeCo format
#' @param trait.data the data frame with species trait data in the TreeCo format
#' @param group vector with the the column that should be used to aggregate the
#'   data. Default to 'ordem'.
#' @param spp.name the name of the columns containing the (morpho)species names.
#'   Default to the TreeCo column 'species.correct'.
#' @param trait.list a vector with the names of the traits should be included in
#'   the calculations.
#' @param rm.flora should the records of (morpho)species not found within the
#'   main sampling but found in the floristics or natural regeneration of the
#'   same survey. Default to TRUE.
#' 
#' @import data.table
#' 
#' @return a data frame with number of individuals (N) and taxa (S) for the each
#'   group and the percentage of N and S with all selected traits (columns
#'   'N.traits' and 'S.traits'), as well as a metric of the completeness of the
#'   traits within each group (columns 'miss.traits.N' and 'miss.traits.S').
#'
#' @details For each selected group, the function calculates the percentage of
#'   individuals and species that have information for all the traits listed in
#'   `trait.list`. This can be used as a measure of the trait coverage within
#'   each group.
#'   
#'   The function also returns a metric of how complete the trait information is
#'   for each group (i.e. survey). This metric is obtained by assigned for each
#'   taxa the number of missing traits, which goes from zero (all traits are
#'   available) to the maximum number of traits listed in `trait.list`. These
#'   number of missing traits are standardized by the maximum number of traits,
#'   so they vary between 0 and 1. Finally, these values are weighted by the number
#'   of individuals/taxa of each level of missing traits. That is, this metric
#'   is the weighted average of the number of missing traits per taxa. The closer to zero,
#'   more complete the taxa are in terms of the traits available.
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
#' @seealso \link[TreeCo]{indetSpecies}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export traitCover
#' 
traitCover <- function(tree.data = NULL, trait.data = NULL, group = "ordem", spp.name = "Name_submitted", 
                       trait.list = c("wsg_gcm3","MaxHeight_m","SeedMass_g","extinction","endemism",
                                      "LeafArea","LeafType","dispersal.syndrome","ecological.group"), 
                       rm.flora = TRUE) {

  ## Checking input
  if (is.null(tree.data) | is.null(trait.data))
    stop("Please provide the input species abundance and/or trait data")
  
  ## Removing species not found in the main tree sampling (natural regeneration of floristics) - Is it necessary for trait analysis?
  if (rm.flora)
    tree.data <- tree.data[tree.data$N >= 1,]
  
  ## Defining the column with the grouping variable(s) and taxon names
  tree.data$group.by <- tree.data[, group]
  tree.data$spp.names <- tree.data[, spp.name]
  DT.trees <- data.table::as.data.table(tree.data[, c("group.by", "spp.names", "N")])
  
  trait.data$spp.names <- trait.data[, spp.name]
  trait.data$na.traits <- apply(trait.data[, trait.list], 1, function(x) sum(is.na(x)))
  DT.traits <- data.table::as.data.table(trait.data[, c("spp.names", trait.list, "na.traits")])
  DT <- data.table::merge.data.table(DT.trees, DT.traits, by="spp.names", all.x = TRUE, sort = FALSE)

  low.trait.taxa <- DT.traits[na.traits >= 3, .(spp.names, na.traits)]
  low.trait.taxa <- low.trait.taxa[grepl(" ", spp.names),]
  cat("Species with 3 or more missing traits (may hinder functional diversity analyses): \n",
      knitr::kable(low.trait.taxa, row.names = FALSE, col.names = c("Species", "NA.traits")),"", sep="\n")

  ## Obtaining the total number of records, individuals and species per group
  data.table::setkey(DT, group.by)
  summ.DT <- DT[ , sum(N), by = group.by] # Number of individuals
  data.table::setnames(summ.DT, c("V1"), c("N"))
  summ.DT[, S := DT[ , data.table::uniqueN(spp.names), by = group.by]$V1,] # Number of species
  
  ## Obtaining the number of species and individuals within the selected trait coverage classes
  data.table::setkey(DT, group.by, na.traits)
  tmp.DT <- DT[data.table::CJ(group.by, na.traits, unique = TRUE), sum(N), by = .EACHI]
  maximo <- max(tmp.DT$na.traits, na.rm = TRUE)
  tmp.DT[is.na(na.traits), na.traits := maximo]
  tmp.DT[is.na(V1), V1 := 0]
  data.table::setnames(tmp.DT, c("V1"), c("N.traits"))
  tmp.DT[, S.traits := DT[data.table::CJ(group.by, na.traits, unique = TRUE), 
                         data.table::uniqueN(spp.names), by = .EACHI]$V1, ]
  tmp.DT[N.traits == 0 & S.traits == 1, S.traits := 0]

  ## Calculating the number of individuals and species with all traits
  data.table::setkey(tmp.DT, group.by)
  all.DT <- tmp.DT[na.traits == 0, lapply(.SD, sum), 
                   by = group.by, .SDcols = c("N.traits", "S.traits")]
  summ.DT <- data.table::merge.data.table(summ.DT, all.DT, by="group.by", all.x = TRUE, sort = FALSE)
  
  ## Calculating the weighted average of the standardized classes of trait coverage (0 = all traits, 1 = all traits missing)
  tmp.DT[ , na.traits := na.traits / maximo]
  wmean.N <- tmp.DT[, lapply(.SD, weighted.mean, w = N.traits), 
                     by = group.by, .SDcols= c("na.traits")]
  data.table::setnames(wmean.N, c("na.traits"), c("miss.traits.N"))
  summ.DT <- data.table::merge.data.table(summ.DT, wmean.N, by="group.by", all.x = TRUE, sort = FALSE)
  wmean.S <- tmp.DT[, lapply(.SD, weighted.mean, w = S.traits), 
                    by = group.by, .SDcols= c("na.traits")]
  data.table::setnames(wmean.S, c("na.traits"), c("miss.traits.S"))
  summ.DT <- data.table::merge.data.table(summ.DT, wmean.S, by="group.by", all.x = TRUE, sort = FALSE)
  
  ## Getting the percentages
  summ.DT[ , c("N.traits") := lapply(.SD, function(x) round(100 * x/N, 4)),
             .SDcols = c("N.traits")]
  summ.DT[ , c("S.traits") := lapply(.SD, function(x) round(100 * x/S, 4)),
           .SDcols = c("S.traits")]
  
  ## Preparing to return
  data.table::setnames(summ.DT, c("group.by"), c(group))
  return(as.data.frame(summ.DT))
}
