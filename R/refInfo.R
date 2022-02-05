#' @title Extract Information from References
#'
#' @description Function the compile different information (e.g. authors, year 
#' of publication) from a reference in the standard TreeCo format
#'
#' @param ref.data the data frame with references in the TreeCo format
#' @param ref.name the name of the columns containing the reference. Default to 
#'   the TreeCo column 'Reference'.
#' @param ref.id the name of the columns containing the reference. Default to 
#'   the TreeCo column 'refID'.
#' @param ref.type a vector containing the type of reference (i.e. journal
#'   article, book, book chapter, monography). If not provide, it assumes that
#'   all (missing) references are journal articles.
#' 
#' @importFrom  plantR prepName lastName
#' @importFrom stringr str_squish
#' 
#' @return a data frame with number with the reference information in each
#'   column.
#'
#' @details The standard TreeCo format for the references is the following:
#' Author(s). Year. Title. Publication name volume (issue): start page-endpage.
#' The separation between multiple authors must be done using semi-colons,
#' ampersand (i.e. '&'), a vertical pipe (i.e. '|')or the word 'and' surrounded
#' by spaces in English, Portuguese, Spanish, German or Latin (i.e. ' and ', 
#' ' e ' ', ' y ', ' und ', ' et ').
#'   
#'   
#' @seealso \link[TreeCo]{}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export refInfo
#' 
#' 
refInfo <- function(ref.data = NULL, 
                    ref.name = "Reference", 
                    ref.id = "refID", 
                    ref.type = NULL) {
  
  ## Checking input
  if (is.null(ref.data))
    stop("Please provide the input reference")
  
  if (!ref.name %in% names(ref.data))
    stop("Please provide the column name where references are stored")
  
  if (!is.null(ref.id)) {
    if (!ref.id %in% names(ref.data))
      stop("Please provide a valid column name where reference IDs are stored")
    
  } else {
    ref.data[, ref.id] <- seq_len(dim(ref.data)[1])
  }
  
  if (is.null(ref.type)) {
    ref.type <- rep("journal article", dim(ref.data)[1])
  } else {
    if (length(ref.type) != dim(ref.data)[1])
      stop("Lenght of the reference type vector does not match that of the input data frame")
    ref.type[ref.type %in% c("", " ", NA)] <- "journal article"
  }
  
  refs <- ref.data[, c(ref.id,  ref.name)]
  refs.temp <- ref.data[, ref.name]

  # AUTHORS -----------------------------------------------------------------
  ## extracting all authors
  autores <- stringr::str_squish(
    gsub("[1][9][0-9][0-9].*|[2][0][0-9][0-9].*", "", refs.temp, perl = TRUE))
  autores <- gsub(" \\($", "", autores, perl = TRUE)
  ## unpublished dataset without year
  check_these <- grepl(" \\(unpublished", refs.temp, perl = TRUE, 
                       ignore.case = TRUE)
  autores[check_these] <- 
    gsub(" \\(unpublished.*", "", autores[check_these], perl = TRUE)
  autores[autores %in% c(""," ")] <- NA_character_

  ## first author
  first.aut <- plantR::prepName(autores, fix.names = TRUE, sep.out = " | ",
                                output = "first", special.char = TRUE,
                                format = "init_last", get.prep = TRUE,
                                get.initials = FALSE)
  ## other authors
  other.aut <- plantR::prepName(autores, fix.names = TRUE, sep.out = " | ",
                                output = "aux", special.char = TRUE,
                                format = "init_last", get.prep = TRUE,
                                get.initials = FALSE)
  
  ## first author family name
  last.name <- plantR::lastName(first.aut) 
  first.name <- plantR::lastName(first.aut, invert = TRUE) 

  # #checking
  # tail(sort(table(first.aut[!grepl("no|NE", refs.treeco$status)])),40)
  
  # removing authors from the reference
  refs.temp1 <- mapply(function(x, y) { gsub(x, "", y, fixed = TRUE) },
                               autores, refs.temp)
  refs.temp1.list <- strsplit(refs.temp1, "\\. ", perl = TRUE)
  
  # YEAR ----------------------------------------------------------------------
  anos <- stringr::str_squish(
            unlist(lapply(refs.temp1.list, function(x) x[1])))
  
  ## no date
  anos[!grepl("[0-9]", anos, perl = TRUE)] <- NA_character_
  anos[!grepl("[0-9]", refs.temp1, perl = TRUE)] <- NA_character_
  ano_bom <- !grepl("\\D", anos) & nchar(anos) == 4 & !is.na(anos)
  
  ## data elsewhere than after the authors
  check_these <- grepl("[0-9]", anos) & 
                    nchar(anos) >= 10 &
                      !ano_bom
  tmp <- anos[check_these]
  tmp.list <- strsplit(tmp, " ", perl = TRUE)
  tmp.list <- lapply(tmp.list, gsub, 
                     pattern = ";$", replacement = "", perl = TRUE)
  tmp.list <- lapply(tmp.list, gsub, 
                     pattern = "\\)$", replacement = "", perl = TRUE)

  ano.atual <- as.double(plantR::getYear(Sys.Date()))
  patt.year <- paste(1900:ano.atual, collapse = "|")
  
  tmp.ano <- lapply(tmp.list, function(x) x[grepl(patt.year, x, perl = TRUE)])
  tmp.ano[which(unlist(lapply(tmp.ano, function(x) length(x) == 0)))] <- 
    NA_character_
  check_these1 <- lengths(tmp.ano) > 1 & sapply(tmp, nchar) >= 12
  tmp.ano[check_these1] <- lapply(tmp.ano[check_these1], function(x) x[1]) 
  tmp.ano[lengths(tmp.ano) > 1] <- NA_character_
  anos[check_these] <- unlist(tmp.ano)
  
  ## dates by the end of the reference (brazilian monographies)
  check_these <- !ano_bom & grepl(" f$", anos, perl = TRUE)
  anos[check_these] <- plantR::getYear(anos[check_these])
  # anos[check_these] <- sapply(strsplit(anos[check_these], " ", fixed = TRUE), 
  #                               function(x) x[!grepl("\\D", x) & nchar(x) == 4])
  
  ## multiple studies, same year
  anos1 <- anos
  check_these <- !ano_bom & grepl("[0-9][a-o]$|[0-9]\\)[a-o]$", anos1)
  anos1[check_these] <- 
    gsub("([0-9])([a-o]$)", "\\1", anos1[check_these], perl = TRUE)
  anos1[check_these] <- 
    gsub("([0-9]\\))([a-o]$)", "\\1", anos1[check_these], perl = TRUE)
  
  ## final edits
  anos1[!ano_bom] <- gsub("\\.$", "", anos1[!ano_bom], perl = TRUE)
  anos1[!ano_bom] <- gsub("^\\(", "", anos1[!ano_bom], perl = TRUE)
  anos1[!ano_bom] <- gsub("\\)$", "", anos1[!ano_bom], perl = TRUE)
  anos1[!ano_bom] <- gsub("\\'$", "", anos1[!ano_bom], perl = TRUE)
  anos1[!ano_bom] <- gsub(",$", "", anos1[!ano_bom], perl = TRUE)
  anos1 <- stringr::str_squish(anos1)

  # PUBLICATION DETAILS -------------------------------------------------------
  
  ## empty vector to store the publication details
  pub <- vector("list", length(refs.temp1.list))
  pub[unlist(lapply(pub, is.null))] <- NA_character_
  
  ## standardize publication types and publication details
  ref.type1 <- ref.type
  arts <- grepl("article|artigo|journal|revista", ref.type1, 
                perl = TRUE, ignore.case = TRUE)
  if (any(arts)) {
    ref.type1[arts] <- "journal article"
    pub[arts] <- lapply(refs.temp1.list[arts], function(x) tail(x, 1))
  }

  mono <- grepl("monog|tese|disserta|livre doc|rabalho de conclus", 
                ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(mono)) {
    ref.type1[mono] <- "monography"
    pub[mono] <- lapply(refs.temp1.list[mono], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }

  chapter <- grepl("chapter|capitulo", ref.type1,  
                   perl = TRUE, ignore.case = TRUE)
  if (any(chapter)) {
    ref.type1[chapter] <- "book chapter"
    pub[chapter] <- lapply(refs.temp1.list[chapter], 
                           function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  book <- grepl("book|livro", ref.type1, perl = TRUE, ignore.case = TRUE) & 
            !ref.type1 %in% "book chapter"
  if (any(book)) {
    ref.type1[book] <- "book"
    pub[book] <- lapply(refs.temp1.list[book], 
                           function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  anais <- grepl("anais", ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(anais)) {
    ref.type1[anais] <- "conference proceeding"
    pub[anais] <- lapply(refs.temp1.list[anais], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  report <- grepl("report", ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(report)) {
    ref.type1[report] <- "report"
    pub[report] <- lapply(refs.temp1.list[report], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  pub[lengths(pub) == 0] <- NA_character_
  pub1 <- stringr::str_squish(unlist(pub))
  
  
  # VOLUME AND ISSUE ----------------------------------------------------------
  #### TO DO ####    
  
  # PAGES ---------------------------------------------------------------------
  #### TO DO ####    
  
  # TITLE ---------------------------------------------------------------------
  titulo <- vector("list", length(refs.temp1.list))
  titulo <- stringr::str_squish(
                unlist(lapply(refs.temp1.list, function(x) x[2])))
  
  
  # OUTPUT --------------------------------------------------------------------
  out <- cbind.data.frame(authors = autores,
                          first.author = first.aut,
                          first.author.name = first.name,
                          year = anos1,
                          title = titulo,
                          publication = pub1
                          # publisher = 
                          # volume = ,
                          # issue  = ,
                          # start_page = ,
                          # end_page =   
                          # city =   
                          )
  
  out1 <- cbind.data.frame(ID = ref.data[, ref.id], out, stringsAsFactors = FALSE)
  return(out1)
}
