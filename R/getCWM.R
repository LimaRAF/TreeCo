#' @title Obtain Community Weighted Means
#'
#' @description Calculate the Community Weighted Means (CWM), particularly for
#'   data in the TreeCo format.
#'
#' @param tree.data the data frame with tree abundance data.
#' @param trait.data the data frame with tree species trait data.
#' @param group vector with the the column that should be used to aggregate the
#'   data. Default to 'ordem'.
#' @param spp.name the name of the columns containing the (morpho)species names.
#'   Default to the TreeCo column 'species.correct'.
#' @param tax.rank the name of the columns containing the name taxonomic resolution.
#'   Default to the TreeCo column 'taxon.rank'.
#' @param indets a vector of characters containing the unidentified classes in
#'   `tax.rank`. Default to the TreeCo classes 'unidentified', 'family' and
#'   'genus'.
#' @param trait.list a vector with the names of the traits should be included in
#'   the calculations.
#' @param rm.flora should the records of (morpho)species not found within the
#'   main sampling but found in the floristics or natural regeneration of the
#'   same survey. Default to TRUE.
#' @param ab.metric the abundance metric that should be used to weight the
#'   community-level trait means: 'counts' or 'biomass'. Default to 'counts'.
#' @param treeco.cwm should the palms, ferns and/or shrubs be removed prior to
#'   the calculation of the CWM of wood density, potential height and seed mass?
#'   Default to TRUE.
#' @param habit the name of the columns containing the species habit information
#'   (i.e. 'tree', 'shrub', 'others')
#' @param life.form the name of the columns containing the species life form information
#' (i.e. 'woody_tree', 'woody_vines_and_subshrubs', 'palm', 'tree_fern',
#' 'succulent_tree' or 'palmoids').
#' @param reduce.cats should the categories of dispersal and ecological groups
#' be simplified previous to the analysis? Default to TRUE.
#' 
#' @import data.table
#' @importFrom stats xtabs
#' 
#' @return a data frame with the CWMs for each trait X group combination.
#'
#' @details The function .... TO BE FINISHED
#' 
#' If the argument `treeco.cwm` is TRUE: 
#' - Information on species 'habit' and 'life.form' are necessary for filtering
#' somes species out of the CWM from certain traits.
#' - The CWM for each trait is calculated with a different set of species. For
#' Wood Density, CWM excluded non woody species (e.g. palms and ferns). For Max.
#' Height, CWM excludes shrubs (H and/or DBH typically <4m and 10 cm). Seed mass
#' was calculated without tree ferns."
#' 
#' If the argument `treeco.cwm` is TRUE: 
#' - The function reduces the dispersal syndrome to abiotic (anemochoric and
#' autochoric) and zoochoric; hydrochory is disregarded
#' 
#' @author Renato A. F. de Lima
#'
#' @export getCWM
#' 
getCWM <- function(tree.data = NULL, trait.data = NULL, group = "ordem", spp.name = "Name_submitted", tax.rank = "taxon.rank",
                   trait.list = c("wsg_gcm3","MaxHeight_m","SeedMass_g","extinction","endemism",
                                  "LeafArea","LeafType","dispersal.syndrome","succesional.group","ecological.group"),
                   indets = c("unidentified"), rm.flora = TRUE, treeco.cwm = TRUE, reduce.cats = TRUE,
                   habit = "habito", life.form = "life.form", ab.metric = "counts") {
  
  ## Checking input
  if (is.null(tree.data) | is.null(trait.data))
    stop("Please provide the input species abundance and/or trait data")
  
  if (treeco.cwm) {
    if (length(habit) == 1 & !habit %in% names(trait.data))
      stop("Please provide the trait column where species habits are stored")
  
    if (length(habit) == 1 & !habit %in% names(trait.data))
      stop("Please provide the trait column where species habits are stored")
  }
    
  ## Removing species not found in the main tree sampling (natural regeneration of floristics) - Is it necessary for trait analysis?
  if (rm.flora)
    tree.data <- tree.data[tree.data$N >= 1,]
  
  ## PREPARING THE TREE DATA ##
  ## Defining the column with the grouping variable(s) and taxon names
  tree.data$group.by <- tree.data[, group]
  tree.data$tax.ranks <- tree.data[, tax.rank]
  tree.data$spp.names <- tree.data[, spp.name]
  DT.trees <- data.table::as.data.table(tree.data)
  
  ## Removing unidentified species (all traits are NAs) and those not identified at species level
  if (!is.null(indets)) { 
    DT.trees <- DT.trees[!tax.ranks %in% indets,]
    tmp1 <- dim(tree.data)[1] - dim(DT.trees)[1]
    tmp2 <- round(100*(tmp1/dim(tree.data)[1]),1) 
    warning(paste("The sample has ", tmp1," records (", tmp2, 
                  "%) of unidentified individuals (i.e. taxon rank: ", 
                  paste(indets, collapse = " + "),
                  ") that were removed from the analyses", sep=""))
  }  
  
  ## PREPARING THE TRAIT DATA ##
  ## Defining the column with the taxon names and habit and life form
  trait.data$spp.names <- trait.data[, spp.name]
  if (treeco.cwm) {
    # Habits
    if (sum(as.numeric(trait.data[, habit]), na.rm = TRUE)>=1)
      trait.data$habits <- 
        as.character(factor(trait.data[, habit], levels = c("1","0.5","0","1?","0.5?","check"),
                            labels=c("tree","shrub","others","tree","shrub","tree")))
    # Life forms
    trait.data$life.forms <- 
      as.character(factor(trait.data[, life.form], levels = c("woody_tree","woody_vines_and_subshrubs","palm","tree_fern","succulent_tree","palmoids"),
                          labels=c("woody","non_woody","non_woody","fern","non_woody","non_woody")))
    # Dispersal syndromes
    if(any("dispersal.syndrome" %in% trait.list)) {
      trait.data[,"dispersal.syndrome"] <- 
        gsub("\\|hydrochoric", "", trait.data[,"dispersal.syndrome"])
      trait.data[,"dispersal.syndrome"][grepl("zoochoric", trait.data[,"dispersal.syndrome"])] <- 
        "zoochoric"
      trait.data[,"dispersal.syndrome"][grepl("anemochoric\\|autochoric", trait.data[,"dispersal.syndrome"])] <- 
        "anemochoric"
      if (reduce.cats) 
        trait.data[,"dispersal.syndrome"][grepl("anemochoric|autochoric", trait.data[,"dispersal.syndrome"])] <- 
        "abiotic"
    }
    # Succesional group (categorical version of Ecological groups)
    if(any("succesional.group" %in% trait.list)) {
      
      if (reduce.cats) {
        trait.data[, "succesional.group"][trait.data[, "succesional.group"] %in% c("late_secondary",
                                                                                   "early_secondary",
                                                                                   "climax",
                                                                                   "light_demanding",
                                                                                   "shade_tolerant")] <- "non_pioneer"
        
      } else {
        trait.data[, "succesional.group"][grepl("non_pioneer|shade_tolerant", trait.data[,"succesional.group"])] <- 
          "late_secondary"
        trait.data[, "succesional.group"][trait.data[, "succesional.group"] %in% c("light_demanding")] <- 
          "early_secondary"

      } 
    }
  }  
  DT.traits <- data.table::as.data.table(trait.data)
  # Merging the tree and trait data
  DT <- data.table::merge.data.table(DT.trees, DT.traits, 
                                     by="spp.names", all.x = TRUE, sort = FALSE)
  
  ## SUPPORTING TABLES FOR ANALYSES ##

  ## Traits ##
  tmp <- DT[!duplicated(spp.names), .SD, 
            .SDcols = c("spp.names",trait.list)]
  data.table::setorder(tmp, "spp.names")
  tab_traits <- as.data.frame(tmp[, ..trait.list])
  row.names(tab_traits) <- tmp$spp.names
  if("wsg_gcm3" %in% trait.list)
    tab_traits$wsg_gcm3 <- as.double(as.character(tab_traits$wsg_gcm3))
  if("SeedMass_g" %in% trait.list)
    tab_traits$SeedMass_g <- as.double(as.character(tab_traits$SeedMass_g))
  
  ## Site x Abundance tables ##
  #Abundance
  tab_abund <- as.data.frame.matrix(
    stats::xtabs(DT$DA1 ~ as.character(DT$group.by) + DT$spp.names))
  # data.table solution (not used right now due to the long output format)
  # data.table::setkey(DT, group.by, spp.names)
  # tab_abund <-DT[data.table::CJ(group.by, spp.names, unique = TRUE), sum(DA1), by = .EACHI]
  # tab_abund[is.na(V1), V1 := 0]
  # data.table::setnames(tab_abund, c("V1"), c("DA1"))
  
  #Basal area
  tab_basal <- as.data.frame.matrix(
    stats::xtabs(DT$DoA1 ~ as.character(DT$group.by) + DT$spp.names))
  #removing sites without basal area
  tab_basal <- tab_basal[apply(tab_basal, 1, sum, na.rm = TRUE) > 0, ]

  #Making sure we have a good trait table...
  tab_traits <-
    tab_traits[row.names(tab_traits) %in% colnames(tab_abund), , drop = FALSE]
  tab_traits.ab <- 
    tab_traits[row.names(tab_traits) %in% colnames(tab_basal), , drop = FALSE]

  ## Groups of species ##
  if (treeco.cwm) {
    tmp2 <- table(DT$spp.names, DT$habits)
    tmp3 <- table(DT$spp.names, DT$life.forms)
    if("others" %in% unique(DT$habits) & "shrub" %in% unique(DT$habits)) {
      tab_groups <- cbind.data.frame(
        others = tmp2[, "others"],
        shrub = tmp2[, "shrub"],
        tree = tmp2[, "tree"],
        woody = tmp3[, "woody"],
        fern = tmp3[, "fern"],
        palms = tmp3[, "non_woody"])
    } else {
      tab_groups <- cbind.data.frame(
        tree = tmp2[, "tree"], 
        woody = tmp3[, "woody"], 
        fern = tmp3[, "fern"], 
        palms = tmp3[, "non_woody"])
    }
  }

  ## CALCULATING CWM FOR EACH TRAIT AND EACH INVENTORY  ##
  CWM <- cbind.data.frame(rownames(tab_abund),
                          functcomp.treeco(tab_traits, tab_abund),
                          stringsAsFactors = FALSE)
  colnames(CWM)[1] <- group 
  CWM.ab <- cbind.data.frame(rownames(tab_basal),
                             functcomp.treeco(tab_traits.ab, tab_basal),
                             stringsAsFactors = FALSE)
  colnames(CWM.ab)[1] <- group 
  
  if (treeco.cwm) {
    if ("wsg_gcm3" %in% trait.list) {
      tmp <- tab_groups[tab_groups$woody > 0, ]
      tab_traits1 <- tab_traits[row.names(tab_traits) %in% row.names(tmp), , drop = FALSE]
      tab_abund1 <- tab_abund[, colnames(tab_abund) %in% row.names(tmp), drop = FALSE]
      tab_traits1.ab <- tab_traits.ab[row.names(tab_traits.ab) %in% row.names(tmp), , drop = FALSE]
      tab_abund1.ab <- tab_basal[, colnames(tab_basal) %in% row.names(tmp), drop = FALSE]
      
      CWM.woody <- functcomp.treeco(tab_traits1, tab_abund1)
      CWM.woody.ab <- functcomp.treeco(tab_traits1.ab, tab_abund1.ab)
      
      CWM[,"wsg_gcm3"] <- CWM.woody[,"wsg_gcm3"]
      CWM.ab[,"wsg_gcm3"] <- CWM.woody.ab[,"wsg_gcm3"]  
    }
    
    if ("MaxHeight_m" %in% trait.list) {
      tmp <- tab_groups[tab_groups$tree>0,]
      tab_traits1 <- tab_traits[row.names(tab_traits) %in% row.names(tmp), , drop = FALSE]
      tab_abund1 <- tab_abund[, colnames(tab_abund) %in% row.names(tmp), drop = FALSE]
      tab_traits1.ab <- tab_traits.ab[row.names(tab_traits.ab) %in% row.names(tmp), , drop = FALSE]
      tab_abund1.ab <- tab_basal[, colnames(tab_basal) %in% row.names(tmp), drop = FALSE]
      
      CWM.trees <- functcomp.treeco(tab_traits1, tab_abund1)
      CWM.trees.ab <- functcomp.treeco(tab_traits1.ab, tab_abund1.ab)
      
      CWM[,"MaxHeight_m"] <- CWM.trees[,"MaxHeight_m"]
      CWM.ab[,"MaxHeight_m"] <- CWM.trees.ab[,"MaxHeight_m"]  
    }  
    
    if ("SeedMass_g" %in% trait.list) {
      tmp <- tab_groups[tab_groups$fern%in%0,]
      tab_traits1 <- tab_traits[row.names(tab_traits) %in% row.names(tmp), , drop = FALSE]
      tab_abund1 <- tab_abund[, colnames(tab_abund) %in% row.names(tmp), drop = FALSE]
      tab_traits1.ab <- tab_traits.ab[row.names(tab_traits.ab) %in% row.names(tmp), , drop = FALSE]
      tab_abund1.ab <- tab_basal[, colnames(tab_basal) %in% row.names(tmp), drop = FALSE]
      
      CWM.sperma <- functcomp.treeco(tab_traits1, tab_abund1)
      CWM.sperma.ab <- functcomp.treeco(tab_traits1.ab, tab_abund1.ab)
      
      CWM[,"SeedMass_g"] <- CWM.sperma[,"SeedMass_g"]
      CWM.ab[,"SeedMass_g"] <- CWM.sperma.ab[,"SeedMass_g"]  
    }
  }

  ## Preparing to return ##  
  if (all(ab.metric == "counts"))
    result <- CWM
  if (all(ab.metric == "biomass"))
    result <- CWM.ab
  if ("counts" %in% ab.metric & "biomass" %in% ab.metric)
    result <- merge(CWM, CWM.ab, 
                    by = group, all = TRUE, sort = FALSE, suffixes = c(".N",".BA"))
  
  ## Re-ordering and returnig
  group.lvls <- unique(tree.data$group.by) 
  result <- result[match(result[, group], group.lvls),]
  return(result)
}
