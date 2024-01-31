#' @title Prepare TreeCo Species Abundance Table
#'
#' @description To be included
#'
#' @param tree.data the data frame with tree abundance data.
#' @param spp.name = "species.correct"
#' @param site.name = "SiteCode"
#' @param plot.id = "ordem"
#' @param record.id = "RecordID"
#' @param tax.rank = "taxon.rank"
#' @param N.name = "N"
#' @param AB.name = "AB"
#' @param obs.name = "obs"
#' @param linhas = NULL
#' @param rm.sites = NULL
#' @param rm.plots = NULL
#' @param rm.tax = c("liana","bamboo","herb","exclude")
#' @param rm.dead logical. Should dead individuals be removed? Defaults to TRUE
#' @param rm.cf = FALSE
#' @param rm.infra = FALSE
#' @param replace.synonym = FALSE
#' @param rm.spp character. A list of species names to be removed from the
#'   abundance data.
#' @param rm.extra logical. Should the additional abundance data for
#'   size cutoffs different than the TreeCo preffered cutoff (e.g. DAP>3.2cm,
#'   DAS>3.2 inteade of DAP>5cm) etc. be removed? Defaults to TRUE.
#' @param minN = 1
#' 
#' @return a named list containing new data frame and the notes of the
#'   operations selected and applied to obtain the new data frame.
#'
#' @details User needs to choose the operations he wants to execute
#'   over the original species abundance dataset
#' 
#' @author Renato A. F. de Lima
#'
#' @export prepAbundances
#' 
prepAbundances <- function(tree.data = NULL, 
                           spp.name = "species.correct", site.name = "SiteCode",
                           plot.id = "ordem", record.id = "RecordID",
                           tax.rank = "taxon.rank",
                           N.name = "N", AB.name = "AB", obs.name = "obs",
                           linhas = NULL, rm.sites = NULL,  rm.plots = NULL,
                           rm.tax = c("liana","bamboo","herb","exclude"), 
                           rm.dead = TRUE, rm.cf = TRUE, rm.infra = TRUE, 
                           replace.synonym = TRUE, rm.spp = NULL,  
                           rm.extra = TRUE, minN = 1) 
{
  
  
  ## Checking input
  if (is.null(tree.data))
    stop("Please provide the input species abundance table")
  
  if (!"data.frame" %in% class(tree.data))
    stop("Input needs to be a data frame or equivalent")
  
  result <- as.data.frame(tree.data)
  
  ## Defining the working column names
  sufixo <- ".wrk.temp"
  wrk.spp.name <- paste0(spp.name, sufixo) 
  result[ , wrk.spp.name] <- result[, spp.name]
  wrk.site.name <- paste0(site.name, sufixo) 
  result[ , wrk.site.name] <- result[, site.name]
  wrk.id.name <- paste0(plot.id, sufixo) 
  result[ , wrk.id.name] <- result[, plot.id]
  wrk.rec.id.name <- paste0(record.id, sufixo) 
  result[ , wrk.rec.id.name] <- result[, record.id]
  wrk.tax.rank <- paste0(tax.rank, sufixo) 
  result[ , wrk.tax.rank] <- result[, tax.rank]
  
  #### FILTERING THE SELECTED INVENTORIES ----------------------------------
  ## Creating the object to record the decisions take at each step
  notas <- NULL
  
  if (!is.null(linhas)) { # sites or plots
    
    hits.sites <- sum(unique(result[, wrk.site.name]) %in% linhas, na.rm = TRUE)
    hits.plots <- sum(unique(result[, wrk.id.name]) %in% linhas, na.rm = TRUE)
    
    if (hits.sites > hits.plots)
      coluna.alvo <- wrk.site.name
    
    if (hits.sites < hits.plots)
      coluna.alvo <- wrk.id.name
    
    if (hits.sites == hits.plots)
      coluna.alvo <- wrk.id.name
    
    if (any(grepl("\\|", result[, coluna.alvo], perl = TRUE))) {
      
      ids0 <- length(unique(result[, coluna.alvo]))
      
      patt1 <- paste0("^", linhas, "\\|", collapse="|")
      patt2 <- paste0("\\|", linhas, "\\|", collapse="|")
      patt3 <- paste0("\\|", linhas, "$", collapse="|")
      
      keep_these <- 
        result[, coluna.alvo] %in% linhas |
        grepl(patt1, result[, coluna.alvo], perl = TRUE) |
        grepl(patt2, result[, coluna.alvo], perl = TRUE) |
        grepl(patt3, result[, coluna.alvo], perl = TRUE)
      
      result <- result[keep_these,]
      
      ids1 <- length(unique(result[, coluna.alvo]))
      nota <- paste0(ids0 - ids1,
                     " sites or plots were removed from the original data.")
      if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
      
    } else {
      ids0 <- length(unique(result[, coluna.alvo]))
      
      result <- result[result[, coluna.alvo] %in% linhas,]
      
      ids1 <- length(unique(result[, coluna.alvo]))
      nota <- paste0(ids0 - ids1,
                     " sites or plots were removed from the original data.")
      if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
      
    }    
  }
  
  #### REMOVING THE UNWANTED SITES OR PLOTS --------------------------------
  if (!is.null(rm.sites)) { # sites
    rm.sites1 <- rm.sites[rm.sites %in% unique(result[, wrk.site.name])]
    result <- result[!result[, wrk.site.name] %in% rm.sites1, ]
    nota <- paste0("The following sites were removed from the original data: ",
                   paste0(rm.sites1, collapse = ", "), ".")
    if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  if (!is.null(rm.plots)) { # plot IDs
    if (any(grepl("\\|", result[, wrk.id.name], perl = TRUE))) {
      patt <- paste0("^", rm.plots, "$", collapse="|")
      patt1 <- paste0("^", rm.plots, "\\|", collapse="|")
      patt2 <- paste0("\\|", rm.plots, "\\|", collapse="|")
      patt3 <- paste0("\\|", rm.plots, "$", collapse="|")
      
      remove_these <- 
        grepl(patt, result[, wrk.id.name], perl = TRUE) |
        grepl(patt1, result[, wrk.id.name], perl = TRUE) |
        grepl(patt2, result[, wrk.id.name], perl = TRUE) |
        grepl(patt3, result[, wrk.id.name], perl = TRUE)
      result <- result[!remove_these, ]
      nota <- paste0("The following plot IDs were removed from the original data: ",
                     paste0(rm.plots, collapse = ", "), ".")
      if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
      
    } else {
      result <- result[result[, wrk.id.name] %in% rm.plots, ]
      nota <- paste0("The following plot IDs were removed from the original data: ",
                     paste0(rm.plots, collapse = ", "), ".")
      if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
    }    
  }
  
  #### FILTERING SPECIES COUNT AND BASAL AREA ------------------------------
  
  ##Removing species without abundance or basal area records
  result <- result[!is.na(result[, N.name]) | 
                     !is.na(result[, AB.name]),]
  
  ##Removing species recorded only on the floristic survey, if minN > 0
  if (!is.null(minN)) {
    keepN <- !is.na(result[, N.name]) & result[, N.name] >= minN
    keepAB <- !is.na(result[, AB.name]) & result[, AB.name] > 0 
    keep_these <- keepN | keepAB
    result <- result[keep_these, ]
    # nota <- ""
    # if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  #### FILTERING SPECIFIC GROUPS OF RECORDS ------------------------------
  ##Removing dead individuals and non_species
  if (rm.dead) {
    remove_these <- !result[ , wrk.tax.rank] %in% c("dead")
    result <- result[remove_these, ]
    nota <- "Dead individuals removed from the data"
    if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  remove_these <- !result[ , wrk.tax.rank] %in% c("correction")
  result <- result[remove_these, ]
  
  ##Removing species other than trees (e.g. lianas bamboos, herbs, etc)
  if (!is.null(rm.tax)) {
    patt <- paste(rm.tax, collapse = "|")
    remove_these <- !grepl(patt, result[, obs.name], 
                           perl = TRUE, ignore.case = TRUE)
    result <- result[remove_these,]
    nota <- paste0("Records from species of the following groups were removed: ",
                   paste0(rm.tax, collapse = ", "), ".")
    if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  #### EDITING SPECIES NOMEMCLATURE AND TAXONOMIC LEVELS -------------------
  ## Considering species determined as cf. to be from the species suggested for validation 
  if (rm.cf) {
    for (i in seq_along(wrk.spp.name))
      result[, wrk.spp.name[i]] <- 
        gsub(" cf\\. ", " ", result[, wrk.spp.name[i]], perl = TRUE)
    # nota <- ""
    # if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  if (rm.infra) {
    
    for (i in seq_along(wrk.spp.name))
      result[, wrk.spp.name[i]] <- 
        gsub(" var\\..*| subsp\\..*| f\\..*", "", result[, wrk.spp.name[i]], perl = TRUE)
    
    result[ , wrk.tax.rank][result[ , wrk.tax.rank] %in% 
                              c("variety", "subspecies", "forma")] <- "species"
    # nota <- ""
    # if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }  
  
  
  ## Replacing problematic/unresolved ids in TreeCo for the possible synonyms
  if (replace.synonym) {
    for (i in seq_along(wrk.spp.name)) {
      result[, wrk.spp.name[i]] <- gsub("Citrus X aurantium", "Citrus x limon", 
                                        result[, wrk.spp.name[i]], perl = TRUE, 
                                        ignore.case = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Citrus X limon", "Citrus x limon", 
                                        result[, wrk.spp.name[i]], perl = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Citrus x limon", "Citrus x limon", 
                                        result[, wrk.spp.name[i]], fixed = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Coffea arabica", "Coffea liberica", 
                                        result[, wrk.spp.name[i]], fixed = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Ficus insipida", "Ficus adhatodifolia", 
                                        result[, wrk.spp.name[i]], fixed = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Pisonia zapallo", "Pisonia ambigua", 
                                        result[, wrk.spp.name[i]], fixed = TRUE)
      result[, wrk.spp.name[i]] <- gsub("Seguieria aculeata", "Seguieria americana", 
                                        result[, wrk.spp.name[i]], fixed = TRUE)
    }
    
    #trees$Name_submitted[trees$Name_submitted=="Cybianthus peruvianus"] = "Cybianthus brasiliensis"
    #trees$Name_submitted[trees$Name_submitted=="Lafoensia glyptocarpa"] = "Lafoensia pacari"
    
    # nota <- ""
    # if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  }
  
  ## Removing specific species 
  if (!is.null(rm.spp)) {
    for (i in seq_along(wrk.spp.name))
      result <- result[!result[, wrk.spp.name[i]] %in% rm.spp, ]
    # nota <- ""
    # if (is.null(notas)) notas <- nota else notas <- c(notas, nota)
  } 
  
  ## Removing the sites with additional data for different size cutoffs DAP>3.2cm, DAS>3.2, etc.
  if (!is.null(rm.extra)) {
    remove_these <- 
      !grepl("_D", as.character(result[, wrk.site.name]), perl = TRUE)
    result <- result[remove_these, ]
  }    
  
  ## Re-ordering, cleaning and returning
  
  for (i in seq_along(wrk.spp.name)) {
    working <- wrk.spp.name[i]
    original <- gsub(sufixo, "", working)
    
    replace_these <- result[, original] != result[, working]
    replace_these[is.na(replace_these)] <- FALSE
    if (any(replace_these)) {
      result[replace_these, original] <- result[replace_these, working]
    }
  }
  
  replace_these <- result[, tax.rank] != result[, wrk.tax.rank]
  replace_these[is.na(replace_these)] <- FALSE
  if (any(replace_these)) {
    result[replace_these, tax.rank] <- result[replace_these, wrk.tax.rank]
  }
  
  novas.colunas <- names(result)[grepl(sufixo, names(result))]
  result1 <- result[, -which(names(result) %in% novas.colunas)]
  
  notas <- gsub("\\\n", "",notas)
  notas1 <- paste0(">>>NOTE ", 1:length(notas),": ", notas)
  
  results <- list(result1, notas1)
  names(results) <- c("tree_abundances", "editing_notes")
  return(results)
}
