#' tryTolower
#' 
#' finds capital letters and replaces them with the corresponding lower ones
#' 
#' \code{tryTolower} finds capital letters in \code{testo} and replaces them with the corresponding lower ones
#'
#' @param testo a vector of characters to be normalized
#' @return a vector of normalized characters
#' @author Livio Finos
#' @examples 
#' testo<-c("Anna", "B")
#' tryTolower(testo)
#' 



tryTolower <-
function(testo, ifErrorReturnText=FALSE)
{
   # tryCatch error
   try_error = tryCatch(tolower(testo), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
      testo = tolower(testo) else testo = NA
   testo
}
