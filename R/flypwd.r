#' Returns the password stored by flypwd
#'
#' @name flypwd
#' @export
#' @usage flypwd(service, clean)
#' @param service service name
#' @param clean flush credentials stored in session
#' @author Giuseppe Acito
#' @return the current user's password
#' @include options.r

flypwd <- function(service=NULL, clean=FALSE) {
  service <- ifelse(is.null(service), "default", service)
  key <- paste("flypwd", service, sep="_")
  flypwd_cmd <- tryCatch(file.path(system.file("flypwd", package="rutils", mustWork=TRUE)),
    error = function(cond) "flypwd -p")
  cmd <- paste(flypwd_cmd, service)
  if(clean) {
    setOption(key, NULL)
    cmd <- paste(cmd, "--clean")
  }

  pwd <- getOption(key, NULL)
  if(!is.null(pwd)) {
    return(pwd)
  }

  pwd <- tryCatch({
    system(cmd, intern=TRUE)
  }, warning=function(warn) {
    stop(warn)
  }, error=function(cond) {
    stop(cond)
  })

  if(length(pwd) > 1) {
    pwd <- pwd[-1]
  }
  setOption(key, pwd)
  pwd
}
