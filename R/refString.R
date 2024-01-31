#' @title Build TreeCo Reference String
#'
#' @description Function to build the standard TreeCo reference string based on
#' reference information (i.e. metadata)
#'  
#' @param ref.data the data frame with reference database
#' @param col.names a named list conating the name of the columns needed (see
#'   details)
#' @param default.ref.type a character containing the default type of reference
#'   to be assumed when the reference type is missing. Defaults to 'article'.
#' @param to.upper should the names of the authors be capitalized. Defaults to
#'   FALSE
#' 
#' @importFrom plantR prepName
#' 
#' @return the data frame \code{ref.data} with the standard reference string.
#'
#' @details The standard TreeCo format for the references is the following:
#' AUTHORS(s). Year. Title. Publication name volume (issue): start page-end page.
#' The separation between multiple authors is done using semi-colons.
#'   
#' @seealso \link[TreeCo]{refInfo}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export refString
#' 
#' 
refString <- function(ref.data = NULL, 
                    col.names = NULL,
                    default.ref.type = "article",
                    to.upper = FALSE) {
  
  ## Checking input
  if (is.null(ref.data))
    stop("Please provide the input reference")
  
  if (!"data.frame" %in% class(ref.data))
    stop("Please provide a data frame as an input")
  
  ref.data <- as.data.frame(ref.data)
  
  if (!any(col.names %in% names(ref.data)))
    stop("Please provide the column names matching the input data frame")
  
  # ENTRY TYPES -------------------------------------------------------
  ref.type1 <- as.character(ref.data[,col.names[["entry.type"]]])
  arts <- grepl("article|artigo|journal|revista", ref.type1, 
                perl = TRUE, ignore.case = TRUE)
  if (any(arts)) ref.type1[arts] <- "article"

  mono <- grepl("monog|tese|disserta|livre doc|trabalho de conclus|thesis", 
                ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(mono)) ref.type1[mono] <- "monography"

  chapter <- grepl("chapter|capitulo|incollection", ref.type1,  
                   perl = TRUE, ignore.case = TRUE)
  if (any(chapter)) ref.type1[chapter] <- "incollection"

  book <- grepl("book|livro", ref.type1, perl = TRUE, ignore.case = TRUE) & 
            !ref.type1 %in% "incollection"
  if (any(book)) ref.type1[book] <- "book"
  
  anais <- grepl("anais|inproceedings", ref.type1, 
                 perl = TRUE, ignore.case = TRUE)
  if (any(anais)) ref.type1[anais] <- "inproceedings"

  report <- grepl("report|relatorio|manual", ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(report)) ref.type1[report] <- "techreport"

  replace_these <- !ref.type1 %in% c("article", "monography", "book", 
                                    "incollection", "inproceedings",
                                    "techreport") 
  ref.type1[replace_these] <- default.ref.type

  #Definig the empty vector that will receive the references
  ref.string <- rep(NA, dim(ref.data)[1])
  
  # JOURNAL ARTICLES -------------------------------------------------------
  # Required fields: author, title, journal, year, volume
  # Optional fields: number, pages, month, doi, note, key
  replace_these <- ref.type1 %in% "article"
  refs <- ref.data[replace_these, ]
  autores <- refs[ , col.names[["author"]]]
  autores <- plantR::prepName(autores, sep.out = "; ", special.char = TRUE,
                              format = "last_init", pretty = TRUE,
                              get.prep = TRUE, get.initials = TRUE)
  if (to.upper)
    autores <- to.upper(autores)
  
  ano <- refs[ , col.names[["year"]]]
  titulo <- refs[ , col.names[["title"]]]
  titulo <- gsub("\\.$", "", titulo, perl = TRUE)
  revista <- refs[ , col.names[["journal"]]]
  volume <- refs[ , col.names[["volume"]]]
  numero <- refs[ , col.names[["issue"]]]
  numero[numero %in% c("0", 0)] <- NA_character_
  numero[!is.na(numero)] <- paste0("(",numero[!is.na(numero)],")") 
  volume[!is.na(numero)] <- paste0(volume[!is.na(numero)], 
                                   numero[!is.na(numero)])
  paginas <- refs[ , col.names[["pages"]]]

  ref.string.autor <-
    paste0(autores, " ", ano, ". ",  titulo, ". ", 
           revista, " ", volume, ": ", paginas, ".")
  
  #Replacing bad references (too many missing fields) by NAs
  replace_these1 <- 
    is.na(autores) &  is.na(ano)
  ref.string.autor[replace_these1] <- NA_character_
  replace_these1 <- 
    is.na(autores) &  is.na(revista)
  ref.string.autor[replace_these1] <- NA_character_
  ref.string.autor <- 
    gsub("\\. NA NA\\: NA\\.", ".", ref.string.autor, perl = TRUE)
  ref.string.autor <- 
    gsub("\\. NA\\.", ". ", ref.string.autor, perl = TRUE)
  ref.string[replace_these] <- ref.string.autor

  ## monographies -------------------------------------------------------
  # Required fields: author, title, school, year
  # Optional fields: type, address, month, note, key
  replace_these <- ref.type1 %in% "monography"
  refs <- ref.data[replace_these, ]
  autores <- refs[ , col.names[["author"]]]
  autores <- plantR::prepName(autores, sep.out = "; ", special.char = TRUE,
                              format = "last_init", pretty = TRUE,
                              get.prep = TRUE, get.initials = TRUE)
  if (to.upper)
    autores <- to.upper(autores)
  
  ano <- refs[ , col.names[["year"]]]
  titulo <- refs[ , col.names[["title"]]]
  titulo <- gsub("\\.$", "", titulo, perl = TRUE)
  escola <- refs[ , col.names[["school"]]]
  cidade <- refs[ , col.names[["address"]]]
  paginas <- refs[ , col.names[["pages"]]]
  
  ref.string.mono <-
    paste0(autores, " ", ano, ". ", titulo, ". __TIPO__. ", 
           escola, ", ", cidade, ". ", paginas, "p.")

  #Replacing bad references (too many missing fields) by NAs
  replace_these1 <- is.na(autores) & is.na(ano)
  ref.string.mono[replace_these1] <- NA_character_
  ref.string.mono <- 
    gsub("\\. NA, NA\\. NAp\\.", ".", ref.string.mono, perl = TRUE)
  ref.string.mono <- 
    gsub("\\. NA, NA\\.", ".", ref.string.mono, perl = TRUE)
  ref.string.mono <- 
    gsub("NAp\\.$", ".", ref.string.mono, perl = TRUE)
  
  tipo <- refs[ , col.names[["entry.type"]]]
  mestrado <- tipo %in% "mastersthesis"
  ref.string.mono[mestrado] <- 
    gsub("__TIPO__", "Dissertação (Mestrado)", ref.string.mono[mestrado], 
         perl = TRUE)
  
  doutorado <- tipo %in% "phdthesis"
  ref.string.mono[doutorado] <- 
    gsub("__TIPO__", "Tese (Doutorado)", ref.string.mono[doutorado], 
         perl = TRUE)
  
  monograph <- tipo %in% "monography"
  ref.string.mono[monograph] <- 
    gsub("__TIPO__", "Monography", ref.string.mono[monograph], 
         perl = TRUE)
  
  ref.string[replace_these] <- ref.string.mono
  
  ## books and tech_reports -----------------------------------------------
  replace_these <- ref.type1 %in% c("book", "techreport")
  #### TO BE FINISHED ####
  
  
  ## book chapters -------------------------------------------------------
  # Required fields: author, title, booktitle, publisher, year
  # Optional fields: editor, volume/number, series, type, chapter, pages, address, edition, month, note, key
  replace_these <- ref.type1 %in% "incollection"
  #### TO BE FINISHED ####
  
  
  ## conference proceedingss --------------------------------------------
  # Required fields: author, title, booktitle, year
  # Optional fields: editor, volume/number, series, pages, address, month, organization, publisher, note, key
  replace_these <- ref.type1 %in% "inproceedings"
  #### TO BE FINISHED ####
  
  
  ##Returning -----
  ref.string <- gsub("^ | $", "", ref.string, perl = TRUE)
  ref.string <- gsub("\\s+", " ", ref.string, perl = TRUE)
  ref.string <- gsub("\\. \\.$", ".", ref.string, perl = TRUE)
  ref.data1 <- cbind.data.frame(ref.data, ref.string)
  
  return(ref.data1)
}
