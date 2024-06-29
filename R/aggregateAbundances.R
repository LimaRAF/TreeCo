#' @title Aggregate TreeCo Species Abundance Table per Group
#'
#' @description by ordem or SiteCode
#'
#' @param tree.data a data frame with tree abundance data.
#' @param site.data a data frame with site and plot metadata (optional).
#' @param spp.group a column with the species names to be used for aggregation.
#' @param site.group a column with the site or plot codes/names to be used for aggregation.
#' @param cols.to.sum a vector with the names of the columns in `tree.data` to
#'   be summed over species and groups (generally the numerical variables)
#' @param cols.to.paste a vector with the names of the columns in `tree.data` to
#'   be be pasted over species and groups (generally the categorical variables)
#' @param effort the name of the columns in `site.data` containg the effort of
#'   each inventory in hectares
#' 
#' @importFrom dplyr left_join
#' @importFrom utils head
#' @importFrom data.table data.table setnames setkeyv merge.data.table uniqueN
#' 
#' @return a data frame with the aggregated information per species name per
#'   group (e.g. plot ID or site code)
#'
#' @details Users ...
#' 
#' @author Renato A. F. de Lima
#'
#' @export aggregateAbundances
#' 
aggregateAbundances <- function(tree.data = NULL,
                                site.data = NULL,
                                spp.group = "species.correct",
                                site.group = "ordem",
                                cols.to.sum = c("N", "AB", "DA", "DoA", "DR", 
                                                "DoR", "F", "IVC", "IVI"),
                                cols.to.paste = c("ordem", "refID", "SiteCode", 
                                                  "SubSite", "Name_submitted", "family",
                                                  "genus", "epiteth", "infra.epiteth",
                                                  "authorship", "species.correct",
                                                  "taxon.rank", "family.original",
                                                  "genus.original", "species.original", 
                                                  "author.original","notes",
                                                  "accession","coletor","number",
                                                  "DetBy","DetDate", "obs"),
                                effort = "effort_ha"
) {
  
  ## Checking input
  if (is.null(tree.data))
    stop("Please provide the input species abundance table")
  
  if (!"data.frame" %in% class(tree.data))
    stop("Input needs to be a data frame or equivalent")

  if (!spp.group %in% colnames(tree.data) | 
      !site.group %in% colnames(tree.data))
    stop("Columns with the species and groups not found")

  if (any(!cols.to.sum %in% colnames(tree.data))) {
    miss.cols <- cols.to.sum[!cols.to.sum %in% colnames(tree.data)]
    cols.to.sum <- cols.to.sum[!cols.to.sum %in% miss.cols]
    warning("Some columns to sum were not found in the input data frame: ", 
            paste(miss.cols, collapse = ", "), call. = FALSE)
  }
  
  if (any(!cols.to.paste %in% colnames(tree.data))) {
    miss.cols <- cols.to.paste[!cols.to.paste %in% colnames(tree.data)]
    cols.to.paste <- cols.to.paste[!cols.to.paste %in% miss.cols]
    warning("Some columns to paste were not found in the input data frame: ", 
            paste(miss.cols, collapse = ", "), call. = FALSE)
  }
    
  ## Obtaining the total sums and unique characters per species per inventory
  DT <- data.table::data.table(tree.data)
  data.table::setnames(DT, spp.group, "spp.group.by")
  data.table::setnames(DT, site.group, "site.group.by")
  data.table::setkeyv(DT, c("site.group.by", "spp.group.by"))
  
  to_numeric <- function(x) { suppressWarnings(as.numeric(x)) }
  DT[ , (cols.to.sum) := lapply(.SD, to_numeric), .SDcols = cols.to.sum]
  soma <- DT[ , lapply(.SD, sum, na.rm = TRUE), by = c("site.group.by", "spp.group.by"),
              .SDcols = cols.to.sum]
  cols.total <- paste0(head(cols.to.sum, 2), "total")
  N.AB.cols <- head(cols.to.sum, 2)
  somaTotal <- DT[ , lapply(.SD, sum, na.rm = TRUE), by = c("site.group.by"),
                   .SDcols = N.AB.cols]
  data.table::setnames(somaTotal, N.AB.cols, cols.total)
  soma <- data.table::merge.data.table(soma, somaTotal)
  
  cols.to.paste1 <- cols.to.paste[cols.to.paste %in% names(DT)]
  squish <- function (x) {
    x <- gsub("\\s\\s+", " ", x, perl = TRUE)
    gsub("^ | $", "", x, perl = TRUE)
  }
  DT[ , (cols.to.paste1) := lapply(.SD, squish), .SDcols = cols.to.paste1]
  
  to_paste <- function(x) { paste(unique(x), collapse="|") }
  cola <- DT[ , lapply(.SD, to_paste), by = c("site.group.by", "spp.group.by"),
              .SDcols = cols.to.paste1]
  
  rm_na <- function (x) { 
    x <- gsub("^NA\\|NA\\|NA$", "", x, perl = TRUE)
    x <- gsub("^NA\\|NA$", "", x, perl = TRUE)
    x <- gsub("^NA\\|NA\\|", "", x, perl = TRUE)
    x <- gsub("\\|NA\\|NA$", "", x, perl = TRUE)
    gsub("^NA\\||\\|NA$", "", x, perl = TRUE)
  }  
  cola[ , (cols.to.paste1) := lapply(.SD, rm_na), .SDcols = cols.to.paste1]

  trees1 <- data.table::merge.data.table(soma, cola, 
                                         by = c("site.group.by", "spp.group.by"))
  data.table::setnames(trees1, "spp.group.by", spp.group)
  data.table::setnames(trees1, "site.group.by", site.group)

  
  ## Correcting DR and DoR for each site --------------------------------
  ## Variable names still hardcoded
  trees2 <- as.data.frame(trees1)
  trees2 <- 
    trees2[, match(c(names(tree.data), cols.total), names(trees2), nomatch = 0)]
  
  #Missing DR
  check_these <- trees2$DR %in% 0 & !trees2$N %in% 0
  if (any(check_these))
    trees2$DR[check_these] <- 
      round(100*trees2$N[check_these]/trees2$Ntotal[check_these], 5) 

  check_these <- trees2$DoR %in% 0 & !trees2$AB %in% 0
  if (any(check_these))
    trees2$DoR[check_these] <-
      round(100*(trees2$AB[check_these]/trees2$ABtotal[check_these]), 7)
  
  ##Fixing problems for multiple-plots inventories
  multi.sites <- unique(trees2[, site.group][trees2$DR > 100 | 
                                                  trees2$DoR > 100|
                                                  trees2$IVC > 200|
                                                  trees2$IVI > 300])
  check_these <- trees2[, site.group] %in% multi.sites
  if (any(check_these)) {
    trees2$DR[check_these] <- 
      round(100*trees2$N[check_these]/trees2$Ntotal[check_these], 5)
    trees2$DoR[check_these] <- 
      round(100*(trees2$AB[check_these]/trees2$ABtotal[check_these]), 7)
    trees2$IVC[check_these] <- 
      trees2$DR[check_these] + trees2$DoR[check_these]
  }
  if ("DR" %in% names(trees2))
    trees2$DR[is.nan(trees2$DR)] <- NA
  
  if ("DoR" %in% names(trees2))
    trees2$DoR[is.nan(trees2$DoR)] <- NA
  
  if ("IVC" %in% names(trees2))
    trees2$IVC[is.nan(trees2$IVC)] <- NA
  
  #Fixing mising data and other problems
  check_these <- trees2$DoR %in% 0 & 
                  !trees2$DR %in% 0 & 
                  !trees2$IVC %in% 0
  if (any(check_these)) trees2$DoR[check_these] <- 0.001
  
  ### NAO ENTENDI MAIS A CORREÇÃO AQUI: SUBTITUI AB POR DOR??? ANULANDO
  # tmp <- unique(trees2[, site.group][trees2$AB %in% 0 &
  #                               !trees2$DoR %in% 0 &
  #                               !is.na(trees2$DoR)])
  # check_these <- trees2[, site.group] %in% tmp & trees2$AB %in% 0
  # trees2$AB[check_these] <-  trees2$DoR[check_these]
  
  ## Getting DA and DoA based on total effort  -----------------------------
  if (!is.null(site.data)) {
    ## Getting the inventory list
    tmp <- 
      unique(trees2[, site.group][trees2[, site.group] %in% site.data[,site.group]])
    tmp1 <-
      as.character(unique(trees2[, site.group][!trees2[, site.group] %in% 
                                                 site.data[,site.group]]))
    tmp1 <- strsplit(tmp1[grepl("\\|",tmp1)],"\\|")
    tmp2 <- c(as.list(tmp),tmp1)
    lista <- c(tmp, tmp1)
    
    ## Getting the effort per inventory and merging with the abundance data
    effort.tab <-
      cbind.data.frame(toto = sapply(lista, function(x) paste(x, collapse="|")), 
                       toto1 = sapply(lista, 
                                      function(x) 
                                        sum(site.data[ ,effort][site.data[, site.group] %in%
                                                                x], na.rm=TRUE)))
    names(effort.tab) <- c(site.group, effort)
    trees2 <- dplyr::left_join(trees2, effort.tab, by = site.group)
    
    ## Calculating DA (trees ha-1) e DoA (basal area ha-1)
    trees2$DA1 <- trees2$N/trees2[ ,effort]
    trees2$DoA1 <- trees2$AB/trees2[ ,effort]
  }
  
  return(trees2)
}
