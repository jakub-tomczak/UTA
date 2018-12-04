getAvailableMethods <- function(){
  list(utag = "uta-g", utamp1 = "utamp-1", utamp2 = "utamp-2", roruta = "roruta")
}

assert <- function(expression, message)
{
  if(!all(expression))
  {
    stop(if(is.null(message)) "Error" else message)
  }
}
