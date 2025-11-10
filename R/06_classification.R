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
  } else {
    # Fallback basico
    return(rep("unknown", length(names)))
  }
}

#' Classifica luoghi italiani
#' @export
classify_location_it <- function(places, use_legacy = TRUE) {
  if (use_legacy && exists("classificaUtenti") && exists("vocabolarioLuoghi")) {
    return(classificaUtenti(places, vocabolario = vocabolarioLuoghi))
  } else {
    return(rep("unknown", length(places)))
  }
}