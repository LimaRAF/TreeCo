#' 
#' @title Extract Maximum Values From Intervals
#' 
#' @description Get maximum values stored in parentheses or separated by a
#'   underline
#' 
#' @param x a character or vector
#'
#' @return the character `x` with the maximum values
#'  
#' @keywords internal
#'
#' @noRd
#' 
.max_values <- function (x) {
  
  x[grepl("\\(",x, perl = TRUE)] <- 
    sapply(strsplit(x[grepl("\\(", x, perl = TRUE)], 
                    "\\(", perl = TRUE), 
           function(x) as.double(gsub(")", "", x[length(x)], fixed = TRUE)))
  x[grepl("\\_", x, perl = TRUE)] <- 
    sapply(strsplit(x[grepl("\\_", x, perl = TRUE)], "_", fixed = TRUE), 
           function(x) max(as.double(x)))
  return(x)
}

#' 
#' @title Convert Categorical Wood Density to Means
#' 
#' @description The Forest Products Laboratory (1955) classifies wood in
#'   respect to their basic density as: light (0,30-0,36g/cm³), moderately heavy
#'   (0,42-0,50 g/cm³), heavy (0,50-0,60g/cm³) and vey heavy (0,60-0,72 g/cm³).
#'   
#'   Wong (2002) and Bruzos (2009) classify wood density as light (<0.5 g/cm3),
#'   moderately heavy (0.5-0.8 g/cm3), heavy (between 0.8-1.0 g/cm3) and very
#'   heavy (>1.0 g/cm3).
#'   
#'   In TreeCo, these categories of wood density mostly comes from H. Lorenzi's
#'   books, which were adpated here as follows:
#'   - muito leve: 0.30 g/cm³
#'   - leve: 0.45 g/cm³
#'   - moderadamente pesada: 0.65 g/cm³
#'   - pesada: 0.80 g/cm³
#'   - muito pesada: 0.95 g/cm³
#'   
#' 
#' @param x a vector of wood density categories, in portuguese
#'
#' @return the character `x` with the mean values for the categories
#'  
#' @keywords internal
#' 
#' @references 
#' 
#' Bruzos, T. (2009) Propiedades físicas de la madera. Maderas: Ciencia y
#' tecnología. Universidad del Bio, Chile. 11(1):3-18.
#' 
#' Wong, T. M. (2002). A Dictionary of Malaysian Timbers. Revised by Lim, S. C.
#' & Chung, R. C. K. Malayan Forest Record; No. 30. Forest Research Institute
#' Malaysia. Printed in Malaysia by Percetakan Haji Jantan, Kuala Lumpur,
#' Malaysia. pp.201.
#'
#' 
.cat2mean_wsg <- function (x) {
  
  x[grepl("^muito leve$", x, perl=TRUE)] <- "0.3"
  x[grepl("^leve$|^soft_wooded$", x, perl=TRUE)] <- "0.45"
  x[grepl("^madeira perecivel$", x, perl=TRUE)] <- "0.45"
  x[grepl("^moderadamente pesada$", x, perl=TRUE)] <- "0.65"
  x[grep("^muito pesada$", x, perl=TRUE)] <- "0.95"
  x[grep("pesada|dura", x, perl=TRUE)] <- "0.80"
  
  return(x)
}

#' @title Clean Table
#' 
#' @description
#' Simple function to remove rows or columns without any information (all NAs).
#' It take into account all rows and all columns except the fist column.
#' 
#' @param x a data frame.
#'
#' @return The value of `x` with the empty rows and columns removed
#' 
#' @keywords internal
#' 
#' @noRd
#'
.clean_table <- function(x) {
  if (!class(x)[1] %in% "data.frame")
    x <- as.data.frame(x)
  
  if (dim(x)[1] > 0 & dim(x)[2] > 0) {
    x <- x[!apply(x[,-1], 1, function(x) all(is.na(x))), ]
    x <- x[, c(TRUE, !apply(x[,-1], 2, function(x) all(is.na(x))))]
    return(x)  
  } else {
    return(x)
  }
}  
#' 
#' @title Remove Unwanted Spaces
#' 
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'  
#' @keywords internal
#'
#' @noRd
#' 
.squish <- function (x) {
  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  # x <- gsub("  ", " ", x, perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  return(x)
}
#' 
#' @title Extract Unique Non-Empty Elements
#' 
#' @param x a character or vector
#' @param empty a vector of character to be ignored 
#'
#' @return the character `x` without duplicated elements
#'  
#' @keywords internal
#'
#' @noRd
#' 
.unique <- function (x, empty = c("", " ", NA)) {
  x <- unique(x)
  x <- x[!x %in% empty]
  return(x)
}
#' 
#' @title Convert Leaf Rigidity to Ordinal Values
#' 
#' @description  Convert leaf rigidity categories into an ordinal variable 
#'   
#'   -  membranaceous = "1"
#'   -  submembranaceous =  "1.35"
#'   -  subpapyraceous = "1.7"
#'   -  papyraceous =  "2" # lembra papiro
#'   -  pergamentaceous = "2.35"  # consistencia de pergaminho
#'   -  subchartaceous = "2.7"
#'   -  chartaceous = "3" #consistência de cartolina
#'   -  rigid chartaceous = "3.35"
#'   -  subcoriaceous = "3.7"
#'   -  "coriaceous" = "4" # lembra couro
#'   -  rigid coriaceous = "4.35"
#'   
#' 
#' @param x a vector of leaf rigidity categories
#'
#' @return the character `x` with the values for the categories
#'  
#' @keywords internal
#' 
#'
#' 
.cat2mean_LT <- function(x) {
  
  x <- tolower(x)
  
  x[x %in% "membranaceous"] <- "1"
  x[x %in% c("submembranaceous", "rigid membranaceous", 
             "thick membranaceous")] <- "1.35"
  x[x %in% c("subpapyraceous", "(sub)papiraceos")] <- "1.7"
  x[x %in% c("papyraceous", "papiraceous", "papiraceos")] <- "2" # lembra papiro
  x[x %in% c("pergamentaceous", "rigid papyraceous")] <- "2.35"  # consistencia de pergaminho
  x[x %in% c("subchartaceous", "subchartaceous (thinly chartaceous)",
             "subcartaceo", "slightly chartaceous")] <- "2.7"
  x[x %in% c("chartaceous", "cartaceous", "cartaceas",
             "cartacea", "cartaceos", "cartaceo")] <- "3" #consistência de cartolina
  x[x %in% c("rigid chartaceous", "thick chartaceous", 
        "subcoriaceous (thinly coriaceous)", "rigid",
        "thinly coriaceous", "slightly coriaceous")] <- "3.35"
  x[x %in% c("subcoriaceous", "subcoriaceous (rigid chartaceous)", 
        "subcrassa", "subcoriacea", "subcoriaceas", "subcoriaceo",               
        "subcoriaceos")] <- "3.7"
  x[x %in% c("coriaceous", "crassa", "leathery", "rigida, coriacea", 
             "coriaceo", "coriaceos", "more or less coriaceous", 
             "more or less coriaceous (genus)", 
             "usually coriaceous")] <- "4" # lembra couro
  x[x %in% c("rigid coriaceous", "thick coriaceous")] <- "4.35"

  return(x)
}
#' 
#' @title Convert Ecological Groups to Ordinal Values
#' 
#' @description  Convert ecological groups (e.g. pioneer, early secondary, etc.)
#' into an ordinal variable 
#'   
#' 
#' @param x a vector of ecologial groups
#'
#' @return the character `x` with the values for the categories
#'  
#' @keywords internal
#' 
#'
#' 
.cat2mean_EG <- function(x) {
  
  x <- tolower(x)

  x[x %in% c("","higrofita e halofita")] <- NA 
  
  x[x %in% c("pioneer")] <- 1 
  
  x[x %in% c("frequente em associaces primarias","heliofita",
             "light_demanding","oportunist","shade_intolerant")] <- 1.5
  
  x[x %in% c("early_secondary", "esciofita_to_heliofita",
             "heliofita_to_esciofita", "heliofita_to_mesofita",
             "mesofita_to_heliofita", 
             "early_secondary (pioneer/secondary)",
             "secondary")] <- 2
  
  x[x %in% c("heliofita_to_ciofita","ciofita_to_heliofita",
             "esciofita_to_mesofita","higrofita ou mesofita",
             "mesofita","non_pioneer")] <- 2.5
  
  x[x %in% 
      c("ciofita_to_mesofita","esciofita","late_secondary",
        "late_secondary (secondary/climax)","shade_tolerant")] <- 3

  x[x %in% 
      c("ciofita","climax")] <- 4
  
  return(x)
}