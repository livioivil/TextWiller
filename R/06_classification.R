# R/06_classification.R
#' User Classification Functions
#' 

#' Classifica genere per nomi italiani
#' 
#' @param names Vettore di nomi
#' @param use_legacy Usa la funzione originale
#' @return Classificazione di genere
#' @export
classify_gender_it <- function(names, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti")) {
    return(classificaUtenti(names))
  }
  dict <- load_gender_dictionary()
  cleaned <- tolower(trimws(names))
  res <- dict$gender[match(cleaned, dict$name)]
  res[is.na(res)] <- "unknown"
  res
}

#' Classifica luoghi italiani
#' @export
classify_location_it <- function(places, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti") && exists("vocabolarioLuoghi")) {
    return(classificaUtenti(places, vocabolario = vocabolarioLuoghi))
  }
  dict <- load_location_dictionary()
  cleaned <- tolower(trimws(places))
  res <- dict$category[match(cleaned, dict$name)]
  res[is.na(res)] <- "unknown"
  res
}

# Helpers: dizionari in data/
load_gender_dictionary <- function() {
  if (!exists("dizionario_nomi_propri", envir = environment(), inherits = FALSE)) {
    if (exists("dizionario_nomi_propri", envir = .GlobalEnv, inherits = FALSE)) {
      assign("dizionario_nomi_propri", get("dizionario_nomi_propri", envir = .GlobalEnv), envir = environment())
    } else {
      data(dizionario_nomi_propri, package = "TextWiller3", envir = environment())
    }
  }
  dict <- get("dizionario_nomi_propri", envir = environment(), inherits = FALSE)
  data.frame(
    name = tolower(rownames(dict)),
    gender = as.character(dict$categoria),
    stringsAsFactors = FALSE
  )
}

load_location_dictionary <- function() {
  if (!exists("dizionario_luoghi", envir = environment(), inherits = FALSE)) {
    if (exists("dizionario_luoghi", envir = .GlobalEnv, inherits = FALSE)) {
      assign("dizionario_luoghi", get("dizionario_luoghi", envir = .GlobalEnv), envir = environment())
    } else {
      data(dizionario_luoghi, package = "TextWiller3", envir = environment())
    }
  }
  dict <- get("dizionario_luoghi", envir = environment(), inherits = FALSE)
  data.frame(
    name = tolower(rownames(dict)),
    category = as.character(dict$categoria),
    stringsAsFactors = FALSE
  )
}
