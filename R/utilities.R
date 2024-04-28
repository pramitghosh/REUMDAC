call_API = function(fun, ..., authenticated = TRUE)
{
  if(authenticated == TRUE)
    fun(..., authenticator(return_header = TRUE))
  else
    fun(...)
}