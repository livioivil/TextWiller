tryTolower <-
function(testo)
{
   # tryCatch error
   try_error = tryCatch(tolower(testo), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
      testo = tolower(testo) else testo = NA
   testo
}
