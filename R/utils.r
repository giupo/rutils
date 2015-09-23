#' @import testthat XML
NULL

#' Same as *nix command
#'
#' if "username" is set as environment variable, it's preferred.
#' 
#' @name whoami
#' @export
#' @author Giuseppe Acito
#' @return the OS's username

whoami <- function() {
  username <- Sys.getenv("username")
  if(username != "") {
    username
  } else {
    tolower(Sys.info()[["user"]])
  }
}

#' Creates a working directory
#' The user is in charge of deleting it.
#'
#' @name workDir
#' @export
#' @author Giuseppe Acito
#' @return the path of the newly created workdir

workDir <- function(prefix=NULL) {
  if(is.null(prefix)) {
    prefix <- ifelse(.Platform$OS.type=="unix","/tmp/work", "C:\\temp")
  }
  workdir <- file.path(prefix, whoami())
  suppressWarnings(dir.create(workdir, recursive=TRUE))
  workdir
}

#' holds all the tempdirs to be removed when this packages
#' is unloaded
#'
#' @name .tempdirs
#' @rdname tempdirs
#' @title Utils

.tempdirs <- c()

#' this is how a tempdir function sould work, not as the base::workdir does
#'
#' @name tempdir
#' @author Giuseppe Acito
#' @param prefix something to be appended betweeh \code{workDir} and
#'               the random dir generated
#' @export
#' @return the path of the working directory

tempdir <- function(prefix = NULL) {
  workdir <- workDir()
  if(is.null(prefix)) {
    path <- file.path(workdir, .randomString())
  } else {
    path <- file.path(workdir, prefix, .randomString())
  }
  suppressWarnings(dir.create(path, recursive=TRUE))

  if(!path %in% .tempdirs) {
    .tempdirs <- c(.tempdirs, path)
  }
  
  path
}

#' Just like basename without the ext
#' 
#' @name .basename
#' @rdname utils.Rd
#' @author Giuseppe Acito
#' @param path path
#' @export
#' @return just the name of the path

.basename <- function(path) {
  inizio <- regexpr("/[^/]*$", path)+1
  fine <- regexpr("\\.[^\\.]*$", path)-1
  if(fine<inizio || inizio == fine) {
    stop(path, ": malformed path, or I'm a dork")
  }
  substr(path, inizio, fine)
}

#' genera stringhe random
#'
#' @name .randomString
#' @rdname randomString
#' @export 

.randomString <- function(length=8, prefix="") {
  paste0(prefix, paste(
    sample(
      c(0:9, letters, LETTERS),
      length, replace=TRUE),
    collapse=""))
}

#' controlla se una stringa e' nell'altra
#'
#' @name .containsString
#' @rdname constainsString
#' @export

.containsString <- function(stringa, substring)
  as.numeric(regexpr(substring, stringa)) != -1

#' Slides the \code{x} object in \code{n} parts
#'
#' @name slice
#' @export
#' @author Giuseppe Acito
#' @param x the object to be sliced
#' @param n the number of parts
#' @return the list containing the \code{n} parts

slice<-function(x,n) {
  if(n == 0) {
    x
  } else {
    N <- length(x);
    lapply(seq(1,N,n), function(i) x[i:min(i+n-1,N)])
  }
}

#' Redefines all the object in a list, in the parent.frame
#'
#' Objects in the list MUST be named
#'
#' @name unfold
#' @export
#' @author Giuseppe Acito
#' @param x the object to be unfold

unfold <- function(x) {
  for(name in names(x)) {
    assign(name, x[[name]], envir=parent.frame())
  }
}

#' returns \code{TRUE} if the current R sessions runs on a Windows system
#'
#' @name is.windows
#' @return \code{TRUE} if the current R sessions runs on a Windows system
#' @title Misc Utils
#' @export
#' @author Giuseppe Acito

is.windows <- function() Sys.info()[['sysname']] == "Windows"

#' Checks if system is OSX
#'
#' @name is.darwin
#' @title Misc Utils
#' @usage is.darwin()
#' @return `TRUE` if run on OSX, `FALSE` otherwise
#' @export

is.darwin <- function() Sys.info()[['sysname']] == "Darwin"

#' internal function to check if the runtime environment is jenkins.
#'
#' more like an hack.
#'
#' @name is.jenkins
#' @usage is.jenkins()
#' @return `TRUE` if running under Jenkins, `FALSE` otherwise
#' @title Internal Functions
#' @export

is.jenkins <- function() Sys.getenv("BUILD_URL") != ""

#' Esegue il prodotto cartesiano degli array di stringhe passati come argomento
#'
#' @name combine2
#' @usage combine2(..., prefix, sep)
#' @param ... stringhe su cui eseguire il prodotto cartesiano
#' @param prefix eventuale prefisso da applicare
#' @param sep separatore da utilizzare
#' @examples a = c("A", "B")
#'           b = c("1", "2", "3")
#'           combine2(a,b)
#'           # ritorna "A1" "B1" "A2" "B2" "A3" "B3"
#' @export

combine2 <- function(..., prefix = "", sep = "") {
  suppressWarnings(paste0(prefix, levels(interaction(..., sep = sep))))
}
