#' @title Extract Information from References
#'
#' @description Function to parse different reference information (e.g. authors,
#'   year of publication) from a reference string in the standard TreeCo format
#'
#' @param ref.data the data frame with references in the TreeCo format
#' @param ref.name the name of the columns containing the reference. Default to 
#'   the TreeCo column 'Reference'.
#' @param ref.id the name of the columns containing the reference. Default to 
#'   the TreeCo column 'refID'.
#' @param ref.type a vector containing the type of reference (i.e. journal
#'   article, book, book chapter, monography).
#' @param default.ref.type a character containing the default type of reference
#'   to be assumed when the reference type is missing or when it does not match 
#'   one of the `ref.type` classes. Defaults to 'article'.
#'   
#' @importFrom plantR prepName lastName
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
#' @seealso \link[TreeCo]{refString}   
#'   
#' @author Renato A. F. de Lima
#'
#' @export refInfo
#' 
#' 
refInfo <- function(ref.data = NULL, 
                    ref.name = "Reference", 
                    ref.id = "refID", 
                    ref.type = NULL,
                    default.ref.type = "article") {
  
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
    if (!is.null(default.ref.type)) {
      ref.type <- rep(default.ref.type, dim(ref.data)[1])
    } else {
      default.ref.type <- NA_character_
      ref.type <- rep(default.ref.type, dim(ref.data)[1])
    }
        
  } else {
    if (length(ref.type) != dim(ref.data)[1])
      stop("Lenght of the reference type vector does not match that of the input data frame")
    if (is.null(default.ref.type))
      default.ref.type <- NA_character_
    ref.type[ref.type %in% c("", " ", NA)] <- default.ref.type
  }
  
  refs <- ref.data[, c(ref.id,  ref.name)]
  refs.temp <- ref.data[, ref.name, drop = TRUE]

  # AUTHORS -----------------------------------------------------------------
  ## extracting all authors
  autores <- .squish(
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
  anos <- .squish(
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
  anos1 <- .squish(anos1)

  # PUBLICATION DETAILS -------------------------------------------------------
  
  ## empty vector to store the all publication details
  pub <- vector("list", length(refs.temp1.list))
  pub[unlist(lapply(pub, is.null))] <- NA_character_
  
  ## standardize publication types and publication details
  ref.type1 <- ref.type
  arts <- grepl("article|artigo|journal|revista", ref.type1, 
                perl = TRUE, ignore.case = TRUE)
  if (any(arts)) {
    ref.type1[arts] <- "article"
    pub[arts] <- lapply(refs.temp1.list[arts], function(x) tail(x, 1))
  }

  mono <- grepl("monog|tese|disserta|livre doc|trabalho de conclus", 
                ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(mono)) {
    ref.type1[mono] <- "monography"
    pub[mono] <- lapply(refs.temp1.list[mono], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }

  chapter <- grepl("chapter|capitulo", ref.type1,  
                   perl = TRUE, ignore.case = TRUE)
  if (any(chapter)) {
    ref.type1[chapter] <- "incollection"
    pub[chapter] <- lapply(refs.temp1.list[chapter], 
                           function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  book <- grepl("book|livro", ref.type1, perl = TRUE, ignore.case = TRUE) & 
            !ref.type1 %in% "incollection"
  if (any(book)) {
    ref.type1[book] <- "book"
    pub[book] <- lapply(refs.temp1.list[book], 
                           function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  anais <- grepl("anais", ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(anais)) {
    ref.type1[anais] <- "inproceedings"
    pub[anais] <- lapply(refs.temp1.list[anais], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  report <- grepl("report", ref.type1, perl = TRUE, ignore.case = TRUE)
  if (any(report)) {
    ref.type1[report] <- "techreport"
    pub[report] <- lapply(refs.temp1.list[report], 
                        function(x) paste(x[-c(1:2)], collapse = ". "))
  }
  
  pub[lengths(pub) == 0] <- NA_character_
  pub1 <- .squish(unlist(pub))
  
  replace_these <- !ref.type1 %in% c("article", "monography", "book", 
                                    "incollection", "inproceedings",
                                    "techreport") 
  # Other types of bibtex entries not currently implemented: 
    # booklet, inbook, manual
  
  ref.type1[replace_these] <- default.ref.type
  pub[replace_these] <- 
    lapply(refs.temp1.list[replace_these], function(x) tail(x, 1))
  
  ## empty vectors to store separetely the publication details
  journal <- volume <- number <- pages <- bookTitle <- publisher <- address <- 
    bookAuthor <- school <- editor <- rep(NA_character_, length(pub1))
  
  ## articles -------------------------------------------------------
  # Required fields: author, title, journal, year, volume
  # Optional fields: number, pages, month, doi, note, key
  replace_these <- ref.type1 %in% "article"
  ### journal volume, number and pages
  journal[replace_these] <-
    .squish(gsub(" [0-9].*", "", pub1[replace_these], perl = TRUE))
  
  tmp <- 
    .squish(gsub(".*[a-zA-Z]", "", pub1[replace_these], 
                             perl = TRUE))
  tmp <- .squish(gsub("\\:", ": ", tmp, perl = TRUE))
  tmp1 <- lapply(tmp, strsplit, ": |, ", perl = TRUE)
  tmp1 <- lapply(tmp1, function(x) x[[1]][!x[[1]] %in% ""])
  tmp2 <- lapply(tmp1, function(x) x[1])
  tmp2.vol <- lapply(tmp2, function(x) 
                  .squish(gsub("\\(.*", "", x, perl = TRUE)))
  volume[replace_these] <- as.character(unlist(tmp2.vol))
  
  tmp2.numb <- lapply(tmp2, function(x) 
                  .squish(gsub(".*\\(", "", x, perl = TRUE)))
  tmp2.numb <- lapply(tmp2.numb, function(x) 
                  .squish(gsub("\\)$", "", x, perl = TRUE)))
  number[replace_these] <- as.character(unlist(tmp2.numb))
  
  tmp3 <- lapply(tmp1, function(x) tail(x, 1))
  tmp3 <- lapply(tmp3, function(x) gsub("\\.$", "", x, perl = TRUE))
  tmp3[tmp3 %in% c(""," ", "character(0)")] <- NA_character_
  pages[replace_these] <- as.character(unlist(tmp3))
  
  ## monographies -------------------------------------------------------
  # Required fields: author, title, school, year
  # Optional fields: type, address, month, note, key
  replace_these <- ref.type1 %in% "monography"
  if (any(replace_these)) {
    tmp1 <- pub1[replace_these]
    tmp1 <- .squish(gsub(", | - ", ". ", tmp1, perl = TRUE))
    tmp2 <- lapply(tmp1, strsplit, "\\. ", perl = TRUE)
    for(i in 1:length(tmp2)) {
      conteudo <- tmp2[i][[1]][[1]]
      if (length(conteudo) == 0)
        tmp2[i][[1]][[1]] <- NA_character_
    }
    
    journal[replace_these] <- 
      as.character(unlist(lapply(tmp2, function(x) x[[1]][1])))
    school[replace_these] <- 
      as.character(unlist(lapply(tmp2, function(x) x[[1]][2])))
    address[replace_these] <- 
      as.character(unlist(lapply(tmp2, function(x) x[[1]][3])))
    pages[replace_these]  <- 
      as.character(unlist(lapply(tmp2, function(x) tail(x[[1]], 1))))      
  }
  
  ## books and tech_reports -----------------------------------------------
  #### NEED TO BE REVISED/OPTIMIZED ####
  # Required fields: author/editor, title, publisher, year
  # Optional fields: volume/number, series, address, edition, month, note, key, url
  replace_these <- ref.type1 %in% c("book", "techreport")
  if (any(replace_these)) {
    tmp1 <- pub1[replace_these]
    tmp1 <- .squish(gsub(", | - ", ". ", tmp1, perl = TRUE))
    tmp2 <- lapply(tmp1, strsplit, "\\. ", perl = TRUE)
    for(i in 1:length(tmp2)) {
      conteudo <- tmp2[i][[1]][[1]]
      if (length(conteudo) == 0)
        tmp2[i][[1]][[1]] <- NA_character_
    }
    publisher[replace_these] <- 
      as.character(unlist(lapply(tmp2, function(x) x[[1]][1])))
    address[replace_these] <- 
      as.character(unlist(lapply(tmp2, function(x) x[[1]][2])))
    pages[replace_these]  <- 
      as.character(unlist(lapply(tmp2, function(x) tail(x[[1]], 1)))) 
  }    
  
  
  ## book chapters -------------------------------------------------------
  # Required fields: author, title, booktitle, publisher, year
  # Optional fields: editor, volume/number, series, type, chapter, pages, address, edition, month, note, key
  replace_these <- ref.type1 %in% c("incollection")
  if (any(replace_these)) {
    tmp1 <- pub1[replace_these]
    tmp1 <- .squish(gsub("\\. \\)", ".)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(ed\\.\\)", "(Ed.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(ed\\)", "(Ed.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(eds\\.\\)", "(Eds.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(eds\\)", "(Eds.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(org\\.\\)", "(Org.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(org\\)", "(Org.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(orgs\\.\\)", "(Orgs.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(orgs\\)", "(Orgs.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(coord\\)", "(Coord.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(coords\\.\\)", "(Coords.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("\\(coords\\)", "(Coords.)", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("^. ", "", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(p) ([0-9])", "\\1. \\2", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(p.)([0-9])", "\\1 \\2", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(vol.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(vol.) ([IVX])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(v.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(v.) ([IVX])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(cap.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(chap.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp2 <- 
      .squish(gsub("\\(Ed\\.\\)|\\(Eds\\.\\)|\\(Org\\.\\)|\\(Orgs\\.\\)|\\(Coord\\.\\)|\\(Coords\\.\\)", 
                               "___", tmp1, perl = TRUE))
    # Case 1: with editor names
    replace_these1 <- grepl("___", tmp2)
    tmp2.1 <-  lapply(tmp2[replace_these1], strsplit, "___", perl = TRUE)
    tmp2.editor <- lapply(tmp2.1, function(x) x[[1]][1])
    tmp2.editor <-  gsub(" In ", " In: ", tmp2.editor, 
                         perl = TRUE, ignore.case = TRUE)
    tmp2.editor <- .squish(gsub(".*n: ", "", tmp2.editor, 
                                            perl = TRUE, ignore.case = TRUE))
    tmp2.editor <- plantR::prepName(tmp2.editor, fix.names = TRUE, sep.out = "; ",
                                    output = "all", special.char = TRUE,
                                    format = "last_init", get.prep = TRUE,
                                    get.initials = FALSE)
    
    tmp2.2 <- lapply(tmp2.1, function(x) x[[1]][2])
    tmp2.3 <- lapply(tmp2.2, strsplit, "\\. | pp\\.| p\\.", perl = TRUE)
    tmp2.3 <- lapply(tmp2.3, function(x) x[[1]][!x[[1]] %in% "pp"])
    tmp2.3 <- lapply(tmp2.3, function(x) x[!x %in% ""])
    tmp2.3[lengths(tmp2.3) == 0] <- NA_character_
    
    tmp2.book <- 
      .squish(as.character(unlist(lapply(tmp2.3, function(x) x[1]))))
    
    tmp2.pages <- 
      .squish(as.character(unlist(lapply(tmp2.3, function(x) tail(x, 1)))))
    tmp2.pages <- gsub("\\.$", "", tmp2.pages, perl = TRUE)
    tmp2.pages <- gsub("^pp \\.", "", tmp2.pages, perl = TRUE)
    
    tmp2.4 <- tmp2.3
    list.numb <- lengths(tmp2.4)
    for (i in 1:length(tmp2.4)) {
      if (list.numb[i] == 3)
        tmp2.4[[i]] <- tmp2.4[[i]][2] 
      if (list.numb[i] == 4)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-4)] 
    }
    
    vol.patt <- "vol\\.[0-9]|v\\.[0-9]|vol\\.[IVX]|v\\.[IVX]"
    tmp2.vol <- lapply(tmp2.4, 
                       function(x) x[grepl(vol.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.vol[lengths(tmp2.vol) == 0] <- NA_character_
    tmp2.vol <- as.character(unlist(tmp2.vol))
    
    chap.patt <- "cap\\.[0-9]|chap\\.[0-9]"
    tmp2.chap <- lapply(tmp2.4, 
                        function(x) x[grepl(chap.patt, x, perl = TRUE, ignore.case = TRUE)])   
    tmp2.chap[lengths(tmp2.chap) == 0] <- NA_character_
    tmp2.chap <- as.character(unlist(tmp2.chap))
    
    other.patt <- paste0(vol.patt, chap.patt, collapse = "|")
    tmp2.other <- lapply(tmp2.4, 
                         function(x) x[!grepl(other.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.other[lengths(tmp2.other) == 0] <- NA_character_
    tmp2.other <- 
      .squish(as.character(unlist(lapply(tmp2.other, function(x) x[1]))))
    tmp2.other <- gsub("^,|,$", "", tmp2.other, perl = TRUE)
    tmp2.other <- gsub(": ", ", ", tmp2.other, perl = TRUE)
    
    tmp2.pub <- .squish(gsub(",.*", "", tmp2.other, perl = TRUE))
    tmp2.address <- .squish(gsub(".*, ", "", tmp2.other, perl = TRUE))
    
    ## saving
    bookTitle[replace_these][replace_these1] <- tmp2.book  
    pages[replace_these][replace_these1] <- tmp2.pages 
    publisher[replace_these][replace_these1] <- tmp2.pub 
    address[replace_these][replace_these1] <- tmp2.address
    editor[replace_these][replace_these1] <- tmp2.editor
    chapter[replace_these][replace_these1] <- tmp2.chap
    volume[replace_these][replace_these1] <- tmp2.vol
    
    # Case 2: without editor names
    tmp2.1 <-  tmp1[!replace_these1]
    tmp2.2 <- lapply(tmp2.1, strsplit, "\\. | pp\\.| p\\.", perl = TRUE)
    tmp2.2 <- lapply(tmp2.2, function(x) x[[1]][!x[[1]] %in% c("pp","Pp")])
    tmp2.2 <- lapply(tmp2.2, function(x) x[!x %in% ""])
    tmp2.2[lengths(tmp2.2) == 0] <- NA_character_
    
    tmp2.book <- 
      .squish(as.character(unlist(lapply(tmp2.2, function(x) x[1]))))
    
    tmp2.pages <- 
      .squish(as.character(unlist(lapply(tmp2.2, function(x) tail(x, 1)))))
    tmp2.pages <- gsub("\\.$", "", tmp2.pages, perl = TRUE)
    tmp2.pages <- gsub("^pp \\.", "", tmp2.pages, perl = TRUE)
    
    tmp2.4 <- tmp2.2
    list.numb <- lengths(tmp2.4)
    for (i in 1:length(tmp2.4)) {
      if (list.numb[i] == 3)
        tmp2.4[[i]] <- tmp2.4[[i]][2] 
      if (list.numb[i] == 4)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-4)] 
      if (list.numb[i] == 5)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-5)] 
      if (list.numb[i] == 6)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-6)] 
    }
    
    vol.patt <- "vol\\.[0-9]|v\\.[0-9]|vol\\.[IVX]|v\\.[IVX]"
    tmp2.vol <- lapply(tmp2.4, 
                       function(x) x[grepl(vol.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.vol[lengths(tmp2.vol) == 0] <- NA_character_
    tmp2.vol <- as.character(unlist(tmp2.vol))
    
    chap.patt <- "cap\\.[0-9]|chap\\.[0-9]"
    tmp2.chap <- lapply(tmp2.4, 
                        function(x) x[grepl(chap.patt, x, perl = TRUE, ignore.case = TRUE)])   
    tmp2.chap[lengths(tmp2.chap) == 0] <- NA_character_
    tmp2.chap <- as.character(unlist(tmp2.chap))
    
    other.patt <- paste0(vol.patt, chap.patt, collapse = "|")
    tmp2.other <- lapply(tmp2.4, 
                         function(x) x[!grepl(other.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.other[lengths(tmp2.other) == 0] <- NA_character_
    tmp2.other <- 
      .squish(as.character(unlist(lapply(tmp2.other, function(x) x[1]))))
    tmp2.other <- gsub("^,|,$", "", tmp2.other, perl = TRUE)
    tmp2.other <- gsub(": ", ", ", tmp2.other, perl = TRUE)
    
    tmp2.pub <- .squish(gsub(",.*", "", tmp2.other, perl = TRUE))
    tmp2.address <- .squish(gsub(".*, ", "", tmp2.other, perl = TRUE))
    
    ## saving
    bookTitle[replace_these][!replace_these1] <- tmp2.book  
    pages[replace_these][!replace_these1] <- tmp2.pages 
    publisher[replace_these][!replace_these1] <- tmp2.pub 
    address[replace_these][!replace_these1] <- tmp2.address
    chapter[replace_these][!replace_these1] <- tmp2.chap
    volume[replace_these][!replace_these1] <- tmp2.vol
  }    
  
  
  ## conference proceedingss --------------------------------------------
  #### TO DO ####
  # Required fields: author, title, booktitle, year
  # Optional fields: editor, volume/number, series, pages, address, month, organization, publisher, note, key
  replace_these <- ref.type1 %in% c("inproceedings")
  if (any(replace_these)) {
    tmp1 <- pub1[replace_these]
    tmp1 <- .squish(gsub("^. ", "", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(p) ([0-9])", "\\1. \\2", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(p.)([0-9])", "\\1 \\2", tmp1, perl = TRUE))
    tmp1 <- .squish(gsub("(vol.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(vol.) ([IVX])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(v.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(v.) ([IVX])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(cap.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("(chap.) ([0-9])", "\\1\\2", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub(" In ", " In: ", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp1 <- .squish(gsub("^In ", "In: ", tmp1, perl = TRUE, ignore.case = TRUE))
    tmp2 <- .squish(gsub(".*n: ", "", tmp1, 
                                     perl = TRUE, ignore.case = TRUE))
    tmp2.1 <- lapply(tmp2, strsplit, "\\. | pp\\.| p\\.", perl = TRUE)
    tmp2.2 <- lapply(tmp2.1, function(x) x[[1]][!x[[1]] %in% c("pp","Pp", "p")])
    tmp2.2 <- lapply(tmp2.2, function(x) x[!x %in% ""])
    tmp2.2[lengths(tmp2.2) == 0] <- NA_character_
    
    tmp2.book <- 
      .squish(as.character(unlist(lapply(tmp2.2, function(x) x[1]))))
    
    tmp2.pages <- 
      .squish(as.character(unlist(lapply(tmp2.2, function(x) tail(x, 1)))))
    tmp2.pages <- gsub("\\.$", "", tmp2.pages, perl = TRUE)
    tmp2.pages <- gsub("^pp \\.", "", tmp2.pages, perl = TRUE)
    
    tmp2.4 <- tmp2.2
    list.numb <- lengths(tmp2.4)
    for (i in 1:length(tmp2.4)) {
      if (list.numb[i] == 3)
        tmp2.4[[i]] <- tmp2.4[[i]][2] 
      if (list.numb[i] == 4)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-4)] 
      if (list.numb[i] == 5)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-5)] 
      if (list.numb[i] == 6)
        tmp2.4[[i]] <- tmp2.4[[i]][c(-1,-6)] 
    }
    
    vol.patt <- "vol\\.[0-9]|v\\.[0-9]|vol\\.[IVX]|v\\.[IVX]"
    tmp2.vol <- lapply(tmp2.4, 
                       function(x) x[grepl(vol.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.vol[lengths(tmp2.vol) == 0] <- NA_character_
    tmp2.vol <- as.character(unlist(tmp2.vol))
    
    chap.patt <- "cap\\.[0-9]|chap\\.[0-9]"
    tmp2.chap <- lapply(tmp2.4, 
                        function(x) x[grepl(chap.patt, x, perl = TRUE, ignore.case = TRUE)])   
    tmp2.chap[lengths(tmp2.chap) == 0] <- NA_character_
    tmp2.chap <- as.character(unlist(tmp2.chap))
    
    other.patt <- paste0(vol.patt, chap.patt, collapse = "|")
    tmp2.other <- lapply(tmp2.4, 
                         function(x) x[!grepl(other.patt, x, perl = TRUE, ignore.case = TRUE)])
    tmp2.other[lengths(tmp2.other) == 0] <- NA_character_
    tmp2.other <- 
      .squish(as.character(unlist(lapply(tmp2.other, function(x) x[1]))))
    tmp2.other <- gsub("^,|,$", "", tmp2.other, perl = TRUE)
    tmp2.other <- gsub(": ", ", ", tmp2.other, perl = TRUE)
    
    tmp2.pub <- .squish(gsub(",.*", "", tmp2.other, perl = TRUE))
    tmp2.address <- .squish(gsub(".*, ", "", tmp2.other, perl = TRUE))
    
    ## saving
    bookTitle[replace_these] <- tmp2.book  
    pages[replace_these] <- tmp2.pages 
    publisher[replace_these] <- tmp2.pub 
    address[replace_these] <- tmp2.address
    chapter[replace_these] <- tmp2.chap
    volume[replace_these] <- tmp2.vol
  }    
  
  
  # TITLES ---------------------------------------------------------------
  titulo <- vector("list", length(refs.temp1.list))
  titulo <- .squish(
                unlist(lapply(refs.temp1.list, function(x) x[2])))
  
  
  # OUTPUT ---------------------------------------------------------------
  bibtext.type <- ref.type1
  replace_these <- bibtext.type %in% "monography" & grepl("estrado", journal)
  bibtext.type[replace_these] <- "mastersthesis"
  replace_these <- bibtext.type %in% "monography" & grepl("outorado", journal)
  bibtext.type[replace_these] <- "phdthesis"
  replace_these <- is.na(bibtext.type)
  bibtext.type[replace_these] <- "misc"
  
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("p\\.$|P\\.$", "", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("p$|P$", "", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("f\\.$", "", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("f$", "", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("^\\.", "", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub(" – ", "-", pages[grepl("\\d", pages)], perl = TRUE))
  pages[grepl("\\d", pages)] <- 
    .squish(gsub("– ", "-", pages[grepl("\\d", pages)], perl = TRUE))
  
  out <- cbind.data.frame(entry.type = bibtext.type,
                          # type = ref.type1,
                          author = autores,
                          first.author = first.aut,
                          first.author.name = first.name,
                          last.author.name = last.name,
                          editor = editor,
                          year = anos1,
                          title = titulo,
                          # publication = pub1
                          journal = journal,
                          booktitle = bookTitle,
                          publisher = publisher,
                          school = school,
                          volume = volume,
                          chapter = chapter,
                          number  = number,
                          address = address,
                          pages = pages)
  
  out1 <- 
    cbind.data.frame(ID = ref.data[, ref.id], out, stringsAsFactors = FALSE)
  return(out1)
}
