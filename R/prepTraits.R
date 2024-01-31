#' @title Prepare TreeCo Species Trait Table
#'
#' @description Clean trait records, get derived information and average the
#' species trait information at family, genus, species and infraspecific levels.
#'
#' @param trait.data the data frame with the raw TreeCo trait table.
#' @param spp.name character. The column name containing the species names.
#'   Defaults to "Name_submitted"
#' @param tax.rank = "taxon.rank"
#' @param trait.status character. The column name containing the entry status.
#'   Defaults to "status"
#' @param trait.list a named vector with the column names of the traits should
#'   be included in the calculations.
#' @param rm.dead logical. Should dead individuals be removed? Defaults to TRUE
#' @param rm.indet logical. Should unidentified individuals be removed? Defaults
#'   to TRUE
#' @param rm.dup logical. Should the duplicated or possibly duplicated entries
#'   (of wood density) be removed? Defaults to TRUE
#' 
#' @return a named list containing the edited and averaged trait data at the
#'   different taxonomic levels and the notes of the operations applied
#'   to obtain the new data frame.
#'
#' @details To be included
#' 
#' @author Renato A. F. de Lima & Lucie Zinger
#'
#' @importFrom data.table as.data.table setnames setkeyv merge.data.table .SD
#' @importFrom MASS rlm
#' @importFrom stats lm na.omit quantile
#'
#' @export prepTraits
#' 
#' 
#' 
prepTraits <- function(trait.data = NULL, 
                       spp.name = "Name_submitted",
                       tax.rank = "taxon.rank",
                       trait.status = "status",
                       trait.list = c(WSG = "wsg_gcm3",
                                      H = "MaxHeight_m",
                                      DBH = "MaxDbh_cm",
                                      MinLL = "MinLeafLength_cm", 
                                      MinLW = "MinLeafWidth_cm", 
                                      MaxLL = "MaxLeafLength_cm", 
                                      MaxLW = "MaxLeafWidth_cm",
                                      LA = "LeafArea",
                                      SM = "SeedMass_g",
                                      EXT = "extinction",
                                      END = "endemism", 
                                      LA = "LeafArea",
                                      LT = "LeafType",
                                      DS = "dispersal.syndrome",
                                      EG = "ecological.group"),
                       rm.dead = TRUE, rm.indet = TRUE, rm.dup = TRUE) 
{
  
  ## Checking input
  if (is.null(trait.data))
    stop("Please provide the input species trait table")
  
  if (!"data.frame" %in% class(trait.data))
    stop("Input needs to be a data frame or equivalent")
  
  result <- as.data.frame(trait.data)
  
  ## Preliminary edits
  if (rm.dead)
    trait.data <- trait.data[!trait.data$taxon.rank %in% c("dead"), ]
  
  if (rm.indet)
    trait.data <- trait.data[!trait.data$taxon.rank %in% c("unidentified"), ]
  
  if (rm.dup)
    trait.data[[trait.list["WSG"]]][grepl("exclude|remove_wsg|check_wsg",
                                trait.data[[trait.status]], perl = TRUE)] <- NA
  
  trait <- trait.data
  empty <- c("", " ", NA)
  
  #### MANAGEMENT OF NUMERICAL TRAITS --------------------------------------
  
  #Potential Adult Height
  coluna <- trait.list["H"] 
  trait[[coluna]] <- .max_values(trait[[coluna]])
  trait[[coluna]][grepl(">3", trait[[coluna]], perl = TRUE)] <- 3.5 #### CHECK HERE: ok? ####
  trait[[coluna]] <- as.double(gsub("\\+", "", trait[[coluna]], perl = TRUE))
  nova.coluna <- paste0("Mean", coluna)
  trait[[nova.coluna]] <- trait[[coluna]]
  
  # Maximum stem diameter
  coluna <- trait.list["DBH"] 
  trait[[coluna]] <- .max_values(trait[[coluna]])
  trait[[coluna]] <- as.double(gsub("\\+|<", "", trait[[coluna]], perl = TRUE))
  nova.coluna <- paste0("Mean", coluna)
  trait[[nova.coluna]] <- trait[[coluna]]
  
  #possible typos on dbh or height digitations
  # plot(trait[[trait.list["H"]]] ~ trait[[trait.list["DBH"]]], log="xy")
  # abline(h=10,v=5,lty=2);abline(h=10,v=50,lty=2)
  
  # Wood density
  coluna <- trait.list["WSG"] 
  trait[[coluna]] <- .cat2mean_wsg(trait[[coluna]])
  trait[[coluna]] <- as.character(trait[[coluna]])
  #obtaining means for intervals
  trait[[coluna]][grepl("\\_", trait[[coluna]], perl = TRUE)] <- 
    sapply(strsplit(trait[[coluna]][grepl("\\_", trait[[coluna]])], "_", perl = TRUE), 
           function(x) mean(as.double(x)))
  trait[[coluna]] <- as.double(trait[[coluna]])
  
  #Leaf Length and Width
  cols <- trait.list[c("MinLL", "MinLW", "MaxLL", "MaxLW")]
  for (i in 1:length(cols)) {
    tmp <- .squish(trait[,cols[i]])
    tmp <- gsub(" ", "", tmp, fixed = TRUE)
    
    check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE)
    if (any(check_these)) {
      #removing extreme (low/high) values
      tmp1 <- strsplit(tmp[check_these], "_", fixed = TRUE)
      replace_these <- 
        sapply(tmp1, function(x) length(grep("\\(|\\)", x, perl = TRUE))>0) & 
        sapply(tmp1, function(x) length(grep("al|ais", x, perl = TRUE)) == 0)
      if (any(replace_these)) {
        tmp2 <- tmp1[replace_these]
        
        if (i < 2)	{ 
          tmp3 <- 
            sapply(sapply(tmp2,
                          function(x) strsplit(x[[1]], "\\(|\\)", perl = TRUE)), 
                   function(x) max(as.double(x), na.rm = TRUE)) 
        } else { 
          tmp3 <- 
            sapply(sapply(tmp2, 
                          function(x) strsplit(x[[1]], "\\(|\\)", perl = TRUE)), 
                   function(x) min(as.double(x), na.rm = TRUE)) 
        }	
        tmp1[replace_these] <- tmp3      
      }

      #obtaining avarages for compound leafs with extremes
      replace_these <- 
        sapply(tmp1, function(x) length(grep("\\(|\\)",x, perl = TRUE)) > 0) & 
        sapply(tmp1, function(x) length(grep("al|ais", x, perl = TRUE)) > 0)
      if (any(replace_these)) {
        tmp2 <- tmp1[replace_these]
        tmp3 <- 
          lapply(tmp2, 
                 function(x) mean(as.double(unlist(strsplit(x, "\\(|\\)", perl = TRUE))), na.rm=T))
        tmp1[replace_these] <- tmp3
      }

      #obtaining avarages for compound leafs without extremes
      replace_these <- 
        sapply(tmp1, function(x) length(grep("\\(|\\)", x, perl = TRUE)) == 0) & 
        sapply(tmp1, function(x) length(grep("al|ais", x, perl = TRUE)) > 0)
      if (any(replace_these)) {
        tmp2 <- tmp1[replace_these]
        tmp3 <- 
          lapply(tmp2, 
                 function(x) mean(as.double(unlist(strsplit(x,"\\(|\\)", perl = TRUE))), na.rm=T))
        tmp1[replace_these] <- tmp3
      }
      
      #obtaining mean for ranges
      tmp2 <- sapply(tmp1, function(x) mean(as.double(x), na.rm=T))
      tmp3 <- as.double(tmp[check_these])
      tmp3[!is.na(tmp2) & is.na(tmp3)] <- tmp2[!is.na(tmp2) & is.na(tmp3)]
      tmp[check_these] <- unlist(tmp3)
      
      #saving the results
      trait[,cols[i]] <- as.numeric(tmp)
    }
  }

  # Mean Leaf area and its variation
  coluna <- trait.list["LA"]
  replace_these <- is.na(trait[[coluna]])
  if (any(replace_these)) { # getting missing LA based on leaf width and length, assuming leafs as ellipses
    trait[[coluna]][replace_these] <- pi * 
      (apply(trait[replace_these, cols[c(1,3)]], 1, mean, na.rm=T))/2 *
      (apply(trait[replace_these, cols[c(2,4)]], 1, mean, na.rm=T))/2  
  }
  
  # Checking possible problems
  #trait[is.na(trait$LeafArea)&!is.na(trait$MinLeafWidth_cm),]
  #trait[is.na(trait$LeafArea)&!is.na(trait$MinLeafLength_cm),]
  trait[[coluna]][is.nan(trait[[coluna]])] <- NA
  
  # Getting the variance of Leaf Area (i.e. plasticity)
  tmp = pi * (trait[[cols[1]]])/2 * (trait[[cols[2]]])/2
  tmp1 = pi * (trait[[cols[3]]])/2 * (trait[[cols[4]]])/2
  trait$LeafAreaVar = (tmp1 - tmp)/trait[[coluna]]
  trait$LeafAreaVar[trait$LeafAreaVar %in% 0] <- NA
  trait$LeafAreaVar[!is.na(trait$LeafAreaVar) & 
                      as.double(trait$LeafAreaVar) < 0] <- 
    abs(trait$LeafAreaVar[!is.na(trait$LeafAreaVar) & 
                            as.double(trait$LeafAreaVar) < 0]) 
  
  
  #### CONTINUAR PADRONIZAÇÃO/GENERALIZAÇÃO DAQUI ####
  
  #Petiole or Petiolule for compound leaves
  trait$petiole_cm[trait$petiole_cm %in% ""] <- NA
  trait$petiolule_cm[trait$petiolule_cm %in% ""] <- NA
  
  #editing values
  tmp <- trait$petiole_cm
  tmp[!is.na(trait$petiolule_cm)] <- trait$petiolule_cm[!is.na(trait$petiolule_cm)]
  tmp[!is.na(tmp) & tmp == "sessile"] <- 0.01
  tmp[!is.na(tmp)&tmp %in% 
        c("subsessile","short petiolilate","sessile or <0.2","sessile or subsessile_less than 0.2")] <- 0.1
  tmp[!is.na(tmp)&tmp %in% 
        c("sessile or short petiolate","sessile or short petiolilate","sessile or subsessile")] <- 0.1
  tmp <- .squish(tmp)
  min.pt <- mean.pt <- max.pt <- tmp 
  
  check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE)
  if (any(check_these)) {
    #removing extreme (low/high) values
    tmp1 <- strsplit(tmp[check_these], "_", fixed = TRUE)
    replace_these <- 
      sapply(tmp1, function(x) length(grep("\\(|\\)", x, perl = TRUE))>0) & 
      sapply(tmp1, function(x) length(grep("al|ais", x, perl = TRUE))==0)
    tmp2 <- tmp1[replace_these]
    tmp3 <- sapply(sapply(tmp2, 
                         function(x) strsplit(x[1],"\\(|\\)", perl = TRUE)), 
                  function(x) max(as.double(x[!x %in% empty]),na.rm=T))
    tmp3[is.infinite(tmp3)] <- NA
    tmp4 <- sapply(sapply(tmp2, 
                         function(x) strsplit(x[2],"\\(|\\)", perl = TRUE)), 
                  function(x) min(as.double(x[!x %in% empty]),na.rm=T))
    tmp4[is.infinite(tmp4)] <- NA
    min.pt[check_these][replace_these] <- tmp3
    max.pt[check_these][replace_these] <- tmp4
    mean.pt[check_these][replace_these] <- 
      apply(cbind(tmp3, tmp4), 1, mean, na.rm = TRUE)
    tmp1[replace_these] <- apply(cbind(tmp3, tmp4), 1, mean, na.rm = TRUE)

    ## INCLUIR MANEIRA DE EXTRAIR O COMPRIMENTO LATERAL/BASAL MÍNIMO E MÁXIMO!!
    
    #obtaining averages for basal, central and lateral petiolules
    replace_these <- 
      sapply(tmp1, function(x) length(grep("\\(|\\)", x, perl = TRUE)) == 0) & 
      sapply(tmp1, function(x) length(grep("al|ais", x, perl = TRUE))>0)
    tmp2 <- tmp1[replace_these]
    #sapply(tmp2,function(x) strsplit(x,"basal|central|lateral|mediano|terminal"))
    tmp3 <- sapply(tmp2, 
                  function(x) mean(as.double(unlist(strsplit(x,"\\(|\\)", perl = TRUE))),na.rm=T))
    
    min.pt[check_these][replace_these] <- tmp3
    max.pt[check_these][replace_these] <- tmp3
    mean.pt[check_these][replace_these] <- 
      apply(cbind(tmp3, tmp3), 1, mean, na.rm = T)
    tmp1[replace_these] <- apply(cbind(tmp3, tmp3), 1, mean, na.rm = T)
    #length(unique(unlist(tmp1)))
    
    #obtaining mean for ranges
    replace_these <- lengths(tmp1) > 1
    tmp2 <- tmp1[replace_these]
    tmp3 <- sapply(tmp2,function(x) min(as.double(x),na.rm=T))
    tmp4 <- sapply(tmp2,function(x) max(as.double(x),na.rm=T))
    tmp3[is.infinite(tmp3)] <- NA
    tmp4[is.infinite(tmp4)] <- NA
    
    min.pt[check_these][replace_these] <- tmp3
    max.pt[check_these][replace_these] <- tmp4
    mean.pt[check_these][replace_these] <- 
      apply(cbind(tmp3, tmp4), 1, mean, na.rm = T)
    tmp1[replace_these] <- apply(cbind(tmp3, tmp4), 1, mean, na.rm = T)
    # length(unique(unlist(tmp1)))
    # sort(unique(unlist(tmp1)))
    
    #saving the results
    tmp1 <- as.double(unlist(tmp1))
    tmp[check_these] <- tmp1
  }
  trait$petiole_cm <- tmp
  
  #Color Number
  trait$ColorNumb <- as.numeric(trait$ColorNumb)
  
  #FruitLength_cm, FruitWidth_cm, FruitDiameter_cm	
  #trait$FruitLength_cm[trait$FruitLength_cm %in% c("<12cm","<1cm","<4cm")]
  
  cols = c("FruitLength_cm", "FruitWidth_cm", "FruitDiameter_cm")
  for (i in 1:length(cols)) {
    tmp <- .squish(trait[,cols[i]])
    tmp <- gsub(" ", "", tmp, fixed = TRUE)
    
    check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE)
    if (any(check_these)) {
      #removing extreme values and getting means from intervals
      replace_these <- 
        sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
        sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) > 0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1, "\\(|\\)", perl = TRUE)
      tmp3 <- sapply(strsplit(sapply(tmp2, 
                                      function(x) x[grep("_", x, perl = TRUE)]), "_", fixed = TRUE), 
                      function(x) mean(as.double(x), na.rm=T))
      tmp[check_these][replace_these] <- tmp3
      
      #removing other extreme values
      replace_these <- 
        sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
        sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) == 0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
      tmp3 <- sapply(tmp2,function(x) x[[length(x)-1]]) #### CHECK HERE: ok or mreove the -1? ####
      tmp[check_these][replace_these] <- as.double(tmp3)
      
      #obtaining averages from intervals
      replace_these <- 
        sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) == 0) & 
        sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) > 0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1, "_", fixed = TRUE)
      tmp3 <- sapply(tmp2, function(x) mean(as.double(x), na.rm=T))
      tmp[check_these][replace_these] <- tmp3
      
      #saving the results
      trait[,cols[i]] = as.double(tmp)
      
    }  
  }
  
  #FruitMass_g
  trait$FruitMass_g[trait$FruitMass_g %in% "<0.1"] = 0.05
  replace_these <- grep("_",trait$FruitMass_g, perl = TRUE)
  if (any(replace_these))
    trait$FruitMass_g[replace_these] <- 
      sapply(strsplit(trait$FruitMass_g[replace_these],"_"),
           function(x) mean(as.double(x),na.rm=T))
  
  trait$FruitMass_g <- as.double(trait$FruitMass_g)
  
  #SeedLength_cm, SeedWidth_cm, SeedDiameter_cm
  trait$SeedLength_cm[trait$SeedLength_cm %in% "<0.1"] <- 0.05
  trait$SeedLength_cm <- gsub("cipsela:","",trait$SeedLength_cm, perl = TRUE)
  trait$SeedWidth_cm[trait$SeedWidth_cm %in% "<0.1"] <- 0.05
  trait$SeedDiameter_cm[trait$SeedDiameter_cm %in% "<0.1"] <- 0.05

  trait$SeedLength_cm[trait$SeedLength_cm %in% "<0.01"] <- 0.005
  trait$SeedWidth_cm[trait$SeedWidth_cm %in% "<0.01"] <- 0.005
  trait$SeedDiameter_cm[trait$SeedDiameter_cm %in% "<0.01"] <- 0.005

  cols <- c("SeedLength_cm", "SeedWidth_cm", "SeedDiameter_cm")
  for (i in 1:length(cols)) {
    tmp <- .squish(trait[,cols[i]])
    tmp <- gsub(" ", "", tmp, fixed = TRUE)
    
    check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE)
    if (any(check_these)) {
      #removing extreme values and getting means from intervals 
      replace_these <- 
        sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
        sapply(tmp[check_these], function(x) length(grep("_",x, fixed = TRUE)) > 0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
      tmp3 <- try(sapply(strsplit(sapply(tmp2, 
                                         function(x) x[grep("_", x, perl = TRUE)]), "_", fixed = TRUE), 
                         function(x) mean(as.double(x),na.rm=T)),TRUE)
      tmp[check_these][replace_these] <- tmp3
      
      #removing other extreme values
      replace_these <- 
        sapply(tmp[check_these],function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
        sapply(tmp[check_these],function(x) length(grep("_", x, fixed = TRUE)) == 0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
      tmp3 <- sapply(tmp2, function(x) x[[length(x)-1]]) #### CHECK HERE: ok or mreove the -1? ####
      tmp[check_these][replace_these] <- as.double(tmp3)
      
      #obtaining averages from intervals
      replace_these <- 
        sapply(tmp[check_these],function(x) length(grep("\\(|\\)",x, perl = TRUE))==0) & 
        sapply(tmp[check_these],function(x) length(grep("_",x, fixed = TRUE))>0)
      tmp1 <- tmp[check_these][replace_these]
      tmp2 <- strsplit(tmp1, "_", fixed = TRUE)
      tmp3 <- sapply(tmp2, function(x) mean(as.double(x),na.rm=T))
      tmp[check_these][replace_these] <- tmp3
      
      #saving the results
      trait[,cols[i]] = as.double(tmp)
    }  
  }
  
  #Replacing seed size by seed diameter when seed lenght is NA for some families with seed more or less globose
  tmp <- c("Annonaceae", "Aquifoliaceae", "Arecaceae", "Burseraceae", 
          "Calophyllaceae", "Chrysobalanaceae", "Erythroxylaceae", 
          "Lauraceae", "Loganiaceae", "Nyctaginaceae", "Olacaceae", 
          "Phyllanthaceae", "Piperaceae", "Primulaceae", 
          "Salicaceae", "Sapotaceae", "Siparunaceae", "Myrtaceae")
  replace_these <- is.na(trait$SeedLength_cm) & is.na(trait$SeedWidth_cm) & 
                      !is.na(trait$SeedDiameter_cm) & trait$family %in% tmp
  trait$SeedLength_cm[replace_these] <- 
    trait$SeedDiameter_cm[replace_these]
  
  ##SeedMass_g
  trait$SeedMass_g[trait$SeedMass_g %in% "<0.1"] <- 0.01
  tmp <- .squish(trait$SeedMass_g)
  tmp <- gsub(" ", "", tmp, fixed = TRUE)
  
  check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE)
  if (any(check_these)) {
    #removing extreme values and getting means from intervals
    replace_these <- 
      sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
      sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) > 0)
    tmp1 <- tmp[check_these][replace_these]
    tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
    tmp3 <- try(sapply (strsplit(sapply(tmp2, 
                                       function(x) x[grep("_", x, invert=TRUE, perl = TRUE)]), "_", fixed = TRUE), 
                       function(x) mean(as.double(x), na.rm=T)),TRUE)
    tmp[check_these][replace_these] <- tmp3
    
    #removing other extreme values
    replace_these <- 
      sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) > 0) & 
      sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) == 0)
    tmp1 <- tmp[check_these][replace_these]
    tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
    tmp3 <- sapply(tmp2, function(x) x[[length(x)-1]])
    tmp[check_these][replace_these] <- as.double(tmp3)
    
    #saving the results
    trait$SeedMass_g <- as.double(tmp)
  }  
  
  ##SeedNumb
  trait$SeedNumb <- .squish(trait$SeedNumb)
  trait$SeedNumb <- gsub("^up_to", "1", trait$SeedNumb, perl = TRUE)
  trait$SeedNumb <- gsub("^ate ", "1_", trait$SeedNumb, perl = TRUE)
  trait$SeedNumb <- gsub("^few$", "1_3", trait$SeedNumb, perl = TRUE)
  trait$SeedNumb <- gsub("^ca\\.", "", trait$SeedNumb, perl = TRUE)
  trait$SeedNumb <- gsub(">", "", trait$SeedNumb, perl = TRUE)
  #trait$SeedNumb <- gsub("varias", "6", trait$SeedNumb, perl = TRUE)
  #trait$SeedNumb[grep("multi",trait$SeedNumb)]
  
  tmp <- trait$SeedNumb
  #Extracting means
  check_these <- grepl("media", tmp, perl = TRUE)
  if (any(check_these)) {
    tmp1 <- tmp[check_these]
    tmp2 <- strsplit(tmp1, "media", fixed = TRUE)
    tmp3 <- sapply(tmp2,function(x) as.double(gsub("\\)|_","",x[2])))
    tmp[check_these] <- round(tmp3, 1)
  }
  
  #removing extreme values
  check_these <- grepl("_|\\(|\\)", tmp, perl = TRUE) 
  if (any(check_these)) {
    replace_these <- 
      sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE))>0) & 
      sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE ))==0) 
    tmp1 <- tmp[check_these][replace_these]
    tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
    tmp3 <- sapply (tmp2, function(x) x[1])
    tmp[check_these][replace_these] <- tmp3
    
    #removing other extreme values
    replace_these <- 
      sapply(tmp[check_these], function(x) length(grep("\\(|\\)",x, perl = TRUE)) > 0) & 
      sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) > 0)
    tmp1 <- tmp[check_these][replace_these]
    tmp2 <- strsplit(tmp1,"\\(|\\)", perl = TRUE)
    tmp3 <- sapply (strsplit(sapply(tmp2, 
                                    function(x) x[grep("_", x, fixed = TRUE)]), "_", fixed = TRUE), 
                    function(x) mean(as.double(x),na.rm=T))
    tmp[check_these][replace_these] <- tmp3
    
    #obtaining averages from intervals
    replace_these <- 
      sapply(tmp[check_these], function(x) length(grep("\\(|\\)", x, perl = TRUE)) == 0) & 
      sapply(tmp[check_these], function(x) length(grep("_", x, fixed = TRUE)) > 0)
    tmp1 <- tmp[check_these][replace_these]
    tmp2 <- strsplit(tmp1, "_", fixed = TRUE)
    tmp3 <- sapply(tmp2,function(x) mean(as.double(x),na.rm=T))
    tmp[check_these][replace_these] <- tmp3

  }  
  
  #saving the results
  trait$SeedNumb <- as.double(tmp)
  
  #sem.kg: Getting seed mass form values of seeds per kg
  trait$sem.kg <- gsub(">", "", trait$sem.kg, perl = TRUE)
  replace_these <- grepl("_|;|\\(|\\)", trait$sem.kg, perl = TRUE)
  if (any(replace_these)) {
    tmp <- strsplit(trait$sem.kg[replace_these], "_|;|\\(|\\)", perl = TRUE) #separating range
    tmp <- lapply(tmp, .squish) #removing spaces
    tmp1 <- sapply(tmp, function(x) mean(as.double(x),na.rm=T)) #calculating the mean mass
    tmp1[is.nan(tmp1)] <- NA
    trait$sem.kg[replace_these] <- as.double(tmp1)
  }
  
  replace_these <- is.na(trait$SeedMass_g) & trait$obs1 %in% "sementes/kg"
  trait$SeedMass_g[replace_these] <- 1000/as.double(trait$sem.kg[replace_these])
  
  replace_these <- is.na(trait$SeedMass_g) & trait$obs2 %in% "sementes/kg"
  trait$SeedMass_g[replace_these] <- 1000/as.double(trait$sem.kg[replace_these])

  replace_these <- is.na(trait$SeedMass_g) & trait$obs3 %in% "sementes/kg"
  trait$SeedMass_g[replace_these] <- 1000/as.double(trait$sem.kg[replace_these])
  
  #seed mass ~ seed volume
  trait$SeedVolume <- NA
  #for circular fruits
  replace_these <- is.na(trait$SeedLength_cm) & is.na(trait$SeedWidth_cm) & !is.na(trait$SeedDiameter_cm)
  trait$SeedVolume[replace_these] <- 
    (4/3)*pi*(as.double(trait$SeedDiameter_cm[replace_these])/2)^3
  #for ellipsoidal fruits
  replace_these <- !is.na(trait$SeedLength_cm)&!is.na(trait$SeedWidth_cm)&!is.na(trait$SeedDiameter_cm)
  trait$SeedVolume[replace_these] = 
    (4/3)*pi*(as.double(trait$SeedDiameter_cm[replace_these])/2)*
    (as.double(trait$SeedWidth_cm[replace_these])/2)*
    (as.double(trait$SeedLength_cm[replace_these])/2)
  #plot(log(trait$SeedMass_g) ~ log(trait$SeedVolume)); abline(0,1,lwd=2,col=2)
  #text(x=log(trait$SeedVolume), y=log(trait$SeedMass_g), labels=trait$Name_submitted,cex=0.4,pos=1:3)
  
  tmp <- stats::na.omit(trait[,c("Name_submitted", "SeedMass_g", "SeedVolume")])
  mod <- MASS::rlm(log(as.double(tmp$SeedMass_g)) ~ log(as.double(tmp$SeedVolume)))
  # abline(mod)
  # saveRDS(mod, "seed_mass_vs_volume.rds")
  
  #seed mass ~ fruits mass
  tmp <- stats::na.omit(trait[,c("SeedMass_g", "FruitMass_g", "SeedNumb")])
  tmp <- tmp[tmp$SeedNumb <= 1.5,]
  # plot(log(as.double(tmp$SeedMass_g)) ~ log(as.double(tmp$FruitMass_g)))
  # abline(0,1,lty=2)
  mod1 <- stats::lm(log(as.double(tmp$SeedMass_g)) ~ log(as.double(tmp$FruitMass_g)))
  # abline(mod1,col=2,lwd=2)
  # summary(mod1)
  # saveRDS(mod1, "seed_vs_fruit_mass.rds")
  
  ### A FAZER ###
  #Leaflets => FAZER!!
  #CorollaLength_cm,CorollaWidth_cm

  
  #### MANAGEMENT OF CATEGORICAL TRAITS --------------------------------------
  
  ## Deciduousness
  #OLHAR semideciduous FOR EVERGREEN SPECIES
  trait$Deciduousness[grep("semideciduous", trait$Deciduousness, perl = TRUE)] <-
    "semideciduous" 
  trait$Deciduousness[trait$Deciduousness %in% 
                        c("evergreen?", "presumably evergreen")] <- "evergreen" 
  trait$Deciduousness <- tolower(.squish(trait$Deciduousness))
  
  #Leaf type
  trait$LeafType[grep("simples", trait$LeafType, perl = TRUE)] = "simples" 
  trait$LeafType[grep("compostas unifoliolada", trait$LeafType, perl = TRUE)] = "simples"
  trait$LeafType[grep("compostas", trait$LeafType, perl = TRUE)] = "compostas"
  trait$LeafType[grep("compostas|simples", trait$LeafType,invert=T, perl = TRUE)] = NA
  
  #replacing some NAs values for well known leaf types
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Naucleopsis oblongifolia|Oxandra nitida|Paypayrola blanchetiana|Phyllostemonodaphne geminiflora|Pourouma velutina|Rinorea guianensis|Rinorea bahiensis|Sacoglottis mattogrossensis|Ziziphus joazeiro",trait$species.correct, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Caryodendron janeirense|Ecclinusa ramiflora|Eschweilera ovata|Humiria balsamifera|Neoraputia alba|Pradosia lactescens|Sessea regnellii|Stephanopodium blanchetianum",trait$species.correct, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Eugenia|Myrcia|Casearia|Miconia|Pouteria|Annona|Nectandra|Ocotea|Guapira|Campomanesia|Ilex|Maytenus|Monteverdia|Aspidosperma|Erythroxylum|Psychotria|Myrsine|Chrysophyllum|Solanum|Ficus|Cordia|Licania|Guatteria|Byrsonima|Croton|Hirtella|Virola|Sloanea|Brosimum|Calyptranthes|Mollinedia|Qualea",trait$genus, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Marlierea|Symplocos|Ouratea|Xylopia|Diospyros|Actinostemon|Alseis|Anaxagorea|Antonia|Buchenavia|Chomelia|Cinnamomum|Clusia|Cordiera|Faramea|Gymnanthes|Heisteria|Lacistema|Mabea|Prunus|Styrax|Clarisia|Combretum|Henriettea|Pseudolmedia|Terminalia|Tovomita|Tovomitopsis",trait$genus, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Tibouchina|Aegiphila|Alchornea|Amaioua|Aniba|Cariniana|Celtis|Coccoloba|Couepia|Coussarea|Cryptocarya|Cybianthus|Duguetia|Endlicheria|Eucalyptus|Garcinia|Guettarda|Hyeronima|Ixora|Citharexylum",trait$genus, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Leandra|Licaria|Luehea|Manilkara|Micropholis|Myrceugenia|Parinari|Pera|Persea|Piper|Plinia|Psidium|Randia|Rudgea|Sapium|Simira|Siparuna|Sorocea|Strychnos|Syzygium|Tabernaemontana|Vernonanthura|Vismia|Vochysia|Xylosma",trait$genus, perl = TRUE)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Inga|Acacia|Trichilia|Machaerium|Swartzia|Zanthoxylum|Handroanthus|Guarea|Cupania|Schefflera|Allophylus|Matayba|Tachigali|Senna|Jacaranda|Copaifera|Chamaecrista|Macrolobium|Parkia",trait$genus, perl = TRUE)] = "compostas"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Abarema|Albizia|Anadenanthera|Andira|Astronium|Attalea|Bactris|Cabralea|Cedrela|Ceiba|Cyathea|Dilodendron|Enterolobium|Eriotheca|Erythrina|Euterpe|Geonoma|Hymenaea|Lonchocarpus|Mimosa",trait$genus, perl = TRUE)] = "compostas"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Ormosia|Peltogyne|Picramnia|Piptadenia|Platymiscium|Pseudobombax|Pterocarpus|Senegalia|Syagrus|Tapirira", trait$genus)] = "compostas"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Siphoneugena|Eschweilera|Lecythis", trait$genus)] = "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Conchocarpus fontanesianus|Conchocarpus gaudichaudianus|Conchocarpus pentandrus|Esenbeckia grandiflora", trait$species.correct)] <- "simples"
  trait$LeafType[is.na(trait$LeafType) & 
                   grepl("Tabebuia insignis|Vitex mexiae|Weinmannia humilis", trait$species.correct)] <- "compostas"
  unique(trait$LeafType)
  tail(sort(table(trait$genus[is.na(trait$LeafType)])), 20)
  
  #Check if it is necessary to make the corrections
  #"simples": "Bauhinia rufa", "Pseudobombax simplicifolium", "Roupala montana"

  ##LeafThickness
  tmp <- trait$LeafThickness
  tmp <- .cat2mean_LT(tmp)
  check_these <- grepl(" to | or | a | ou ", tmp, perl = TRUE)   
  if (any(check_these)) {
    tmp1 <- strsplit(tmp[check_these], " to | or | a | ou ", perl = TRUE)
    tmp1 <- lapply(tmp1, .cat2mean_LT)
    tmp1 <- sapply(tmp1, function(x) mean(as.numeric(x), na.rm = TRUE))
    tmp[check_these] <- tmp1
  }  
  trait$LT <- round(as.numeric(tmp), 2)

  ## Dispersal_syndrome
  trait$DispersalSyndrome[trait$DispersalSyndrome %in% "abiotic"] = "non_zoochoric"
  trait$DispersalSyndrome[trait$DispersalSyndrome %in% "check dyspersal in the website"] = NA
  trait$DispersalSyndrome[trait$DispersalSyndrome %in% ""] = NA
  trait$DispersalSyndrome <- tolower(.squish(trait$DispersalSyndrome))
  
  ##SuccesionalGroup: pioneer=1, early_secondary=2,late_secondary=3,climax=4 
  trait$GS <- .cat2mean_EG(trait$SuccesionalGroup)
  trait$GS <- as.numeric(trait$GS)
    
  ##A FINALIZAR
  #"Habit"
  trait$Habit <- tolower(.squish(trait$Habit))
  
  #"latex"
  tmp <- .squish(trait$latex)
  tmp[tmp %in% "sim"] <- "lactescent"
  tmp[tmp %in% c("copious sticky white latex", 
                 "copious flowing white latex",
                 "abundant white latex")] <- "white (abundant)"
  tmp[tmp %in% "red exudate"] <- "red"
  tmp[tmp %in% "branco"] <- "white"
  trait$latex[trait$latex %in% "abundant white or cream-colored latex"] <- 
    "white or cream (abundant)"
      
  #"Bark"
  
  #"LeafSubtype"
  
  #"LeafHairiness_upper","LeafHairiness_lower","LeafMargin"
  
  #"FruitType","FruitShape","PericarpConsistency","FruitColor"
  
  #"DispersalSubsyndrome","MajorDisperser"
  
  #"SexualSystem","PollinationSyndrome","PollinationSubsyndrome","Anthesis"
  
  #"FlowerType","CorollaType","CorollaColour","SepalColour"
  
  #"Flowers","Fruits","Seeds"                 
  
  #"Rewards","Resprouting","ChemicalCompounds","PhysicalCompounds","Allelopathy"
  
  
  #### COMPARING SPECIES NAMES -----------------------------------------------

  # tmp = read.csv('C://Users//renato//Documents//raflima//Pos Doc//Databases//Species abundances//abundances.csv', as.is=T, stringsAsFactors=F)
  # #Species in the abundance data but not at the trait data
  # tmp1= sort(unique(tmp$Name_submitted)[!unique(tmp$Name_submitted) %in% unique(trait$Name_submitted)])
  # tmp1[grepl(" ",tmp1)&!grepl(" var\\. | subsp\\. ",tmp1)]
  # #Genus
  # sort(unique(tmp$genus)[!unique(tmp$genus) %in% unique(trait$genus)])
  # rm(tmp,tmp1)
  
  #### OBTAINING MEAN TRAITS PER TAXONOMICAL LEVEL ----------------------------
  
  # taxon.rank : level at which traits should be aggregated
  trait$taxon.rank_merge <- as.factor(trait$taxon.rank)
  levels(trait$taxon.rank_merge)[match(c("nothosp.","sp.","subspecies","variety"),
                                       levels(trait$taxon.rank_merge))] <- 
    c("genus", "species", rep("infra_species", 2))

  #remove those with no info
  # traits2 = trait[which(trait$taxon.rank!="dead" & 
  #                         trait$taxon.rank!="unidentified" &
  #                         is.na(trait$taxon.rank)==FALSE), ]
  # traits2 <- trait[!is.na(trait$family),]
  traits2 <- droplevels(trait)
  
  #columns indices that are factors or numerics or stuff to ignore
  factors.idx <- 
    which(sapply(1:ncol(traits2), function(x) is.character(traits2[,x])) == T)
  comeca <- which(names(traits2) == "species.original")
  factors.idx <- factors.idx[which(factors.idx > comeca)]
  
  numerics.idx <- 
    which(sapply(1:ncol(traits2), function(x) is.numeric(traits2[,x])) == T)
  numerics.idx <- numerics.idx[which(numerics.idx > comeca)]
  
  #check if these columns are actually factors
  colnames(traits2)[factors.idx]
  # if numeric columns detected here, check in the initial table where is the pb 
  # (or as done in the site table above)
  
  ##Creating the dictionary of mean traits at each taxonomic level
  #creating a list to receive mean trait tables
  taxrank <- levels(traits2$taxon.rank_merge)
  trait.agg <- vector(mode = "list", length =length(taxrank))
  names(trait.agg) <- taxrank
  
  #obtanig mean traits for each taxonomic level
  for (x in taxrank) {
    print(x)
    if(x == "infra_species") {
      tmp = traits2[which(traits2$taxon.rank_merge == x), , drop = F]
      agg = tmp$Name_submitted
    } 
    if(x == "species") {
      tmp = traits2[!is.na(traits2$epiteth), , drop = F]
      agg = paste(tmp$genus, tmp$epiteth, sep = " ")
    } 
    if(x == "genus") {
      tmp = traits2[!is.na(traits2$genus), , drop = F]
      agg = tmp$genus
    } 
    if(x == "family") {
      tmp = traits2[!is.na(traits2$family), , drop = F]
      agg = tmp$family
    }
    
    ## Concatenating characters
    DF <- data.frame(agg, tmp[, factors.idx, drop = FALSE])
    names(DF)[1] <- x
    DT <- data.table::as.data.table(DF)
    data.table::setnames(DT, x, "TAX")
    data.table::setkeyv(DT, "TAX")
    f.concat <- function(x){
                  tutu <- 
                    sort(table(x, useNA = "no"), decreasing=T)
                  result <- 
                    paste(paste(names(tutu), tutu, sep=":"), collapse="|")
                  return(result)
    }
    cols.to.paste <- colnames(tmp)[factors.idx]
    DT.char <- DT[ , lapply(.SD, f.concat), by = c("TAX"),
                .SDcols = cols.to.paste]
    
    ## Averaging numbers
    DF <- data.frame(agg, tmp[, numerics.idx, drop = FALSE])
    names(DF)[1] <- x
    DT <- data.table::as.data.table(DF)
    data.table::setnames(DT, x, "TAX")
    data.table::setkeyv(DT, "TAX")
    cols.to.mean <- colnames(tmp)[numerics.idx]
    cols.to.mean <- cols.to.mean[!cols.to.mean %in% c("MaxHeight_m","MaxDbh_cm")]
    DT.mean <- DT[ , lapply(.SD, mean, na.rm = T), by = c("TAX"),
                              .SDcols = cols.to.mean]

    ## 90% quantile of maximum Height and DBH
    DF <- data.frame(agg, tmp[, c("MaxHeight_m","MaxDbh_cm"), drop = FALSE])
    names(DF)[1] <- x
    DT <- data.table::as.data.table(DF)
    data.table::setnames(DT, x, "TAX")
    data.table::setkeyv(DT, "TAX")
    DT.quant <- DT[ , lapply(.SD, stats::quantile, prob=0.9, na.rm = T), by = c("TAX"),
                   .SDcols = c("MaxHeight_m","MaxDbh_cm")]
    ## Merging the tables
    DT.final <- data.table::merge.data.table(DT.char, DT.mean, by = "TAX")
    DT.final <- data.table::merge.data.table(DT.final, DT.quant, by = "TAX")
    
    trait.agg[[which(names(trait.agg)==x)]] <- DT.final
    
    # out0 = vector(mode="list", length=(length(unique(agg))))
    # names(out0) = unique(agg)
    # #out = do.call("rbind.data.frame", lapply(agg, function(y) {
    # for (y in as.vector(unique(agg))) {
    #   print(match(y, as.vector(unique(agg))))
    #   tmp1 = tmp[which(as.vector(agg) == y), ,drop=F]
    #   #out = droplevels(data.frame(TAX = y, tmp[1,,drop=F]))
    #   out = data.frame(TAX = y, tmp[1,,drop=F])
    #   #collapse factors
    #   out[,factors.idx+1] = sapply(factors.idx, 
    #                                function(z){
    #                                  tutu = sort(table(tmp1[,z],
    #                                                    # useNA = "ifany"),
    #                                                    useNA = "no"),
    #                                              decreasing=T)
    #                                  paste(paste(names(tutu), tutu, sep=":"), collapse="|")
    #                                  
    #                                })
    #   #average 
    #   out[,numerics.idx+1] = sapply(numerics.idx, 
    #                                 function(z) {
    #                                   z1 = tmp1[,z]
    #                                   if(length(table(z1))==0) {NA} else {
    #                                     if(names(tmp1)[z] %in% c("MaxHeight_m","MaxDbh_cm")) {
    #                                       quantile(z1,prob=0.9,na.rm=T) 
    #                                       #max(z1, na.rm=T) 
    #                                     } else {					
    #                                       mean(z1, na.rm=T)
    #                                     }
    #                                   }
    #                                 })
    #   #return useful variables
    #   out0[[which(names(out0)==y)]] = out[,c(1,factors.idx+1, numerics.idx+1)]
    #   #print(y)
    # }
    # trait.agg[[which(names(trait.agg)==x)]] = do.call("rbind.data.frame", out0)
  }
  
  ##Creating a final, unique table with mean traits for all Name_submitted at
  ##the lowest taxonomic resolution available
  #recreate a final table with all Name_submitted and their taxon rank
  tmp.final <- traits2[duplicated(sort(traits2$Name_submitted)) == F, 
                      c("Name_submitted", "taxon.rank_merge")]
  
  #assigning mean trait values by taxon rank
  #### CHECK HERE: ty to make this step faster ####
  cat("lowest")
  traits.final = do.call("rbind.data.frame", 
                         lapply(1:nrow(tmp.final), function(x) {
    tmp = trait.agg[[tmp.final$taxon.rank_merge[x]]]
    x.n = as.vector(tmp.final$Name_submitted[x])
    if (!is.null(tmp[match(x.n, tmp$TAX),])) {
      out = data.frame(tmp.final[x,], tmp[match(x.n, tmp$TAX),])
      return(out)
    }
  }))
  
  ##Saving the results
  # write.csv(traits.final,"traits.lowest.csv")
  # write.csv(trait.agg[["family"]],"traits.family.csv")
  # write.csv(trait.agg[["genus"]],"traits.genus.csv")
  # write.csv(trait.agg[["species"]],"traits.species.csv")
  # write.csv(trait.agg[["infra_species"]],"traits.infra_species.csv")
  
  results <- list(traits.final, 
                  trait.agg[["family"]], 
                  trait.agg[["genus"]], 
                  trait.agg[["species"]], 
                  trait.agg[["infra_species"]],
                  mod, mod1)
                  #notas1)
  names(results) <- c("lowest", "family", "genus", "species", "infra_species",
                      "seed_mass_vs_volume", "seed_vs_fruit_mass")
  return(results)
}
