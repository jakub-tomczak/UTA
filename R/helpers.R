getAvailableMethods <- function(){
  list(utag = "uta-g", utamp1 = "utamp1-g", utamp2 = "utamp2-g", roruta = "roruta")
}

assert <- function(expression, message)
{
  if(!all(expression))
  {
    stop(if(is.null(message)) "Error" else message)
  }
}
