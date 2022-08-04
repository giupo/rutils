#' base::readLines error reporting sucks.
#'
#' @name readLines
#' @usage readLines(con, n, ok, warn, encoding)
#' @seealso base::readLines
#' @param con @see base::readLines
#' @param n @see base::readLines
#' @param ok @see base::readLines
#' @param warn @see base::readLines
#' @param encoding @see base::readLines
#' @export

readLines <- function(con=stdin(), n = -1L, ok =  TRUE,  #nolint
                      warn = TRUE, encoding = "unknown") {
  tryCatch({
    suppressWarnings(
      base::readLines(
        con = con, n = n, ok = ok, warn = warn, encoding = encoding))
    }, error = function(cond) {
      if (is.character(con)) {
        stop('"', con, '": ', cond)
      } else {
        stop(cond)
      }
    })
}

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
#'
#' Creates a temp working directory;
#' The user is in charge of deleting it.
#'
#' @name workDir
#' @usage workDir(prefix)
#' @param prefix prefix to prepend to the directory name (default = NULL)
#' @export
#' @author Giuseppe Acito
#' @return the path of the newly created workdir

workDir <- function(prefix = NULL) { #nolint
  if(is.null(prefix)) {
    prefix <- ifelse(is.windows(), "C:\\temp", "/tmp/work")
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
#' @rdname utils
#' @author Giuseppe Acito
#' @param path path
#' @export
#' @return just the name of the path

.basename <- function(path) { # nolint
  tools::file_path_sans_ext(basename(path))
}

#' genera stringhe random
#'
#' @name .randomString
#' @usage .randomString(length, prefix)
#' @param length lunghezza della stringa (default = 8)
#' @param prefix evantuale prefisso (default = "")
#' @rdname randomString
#' @return stringa con caratteri random di lunghezza = `length` 
#'    ed un prefisso = `prefix`
#' @export

.randomString <- function(length = 8, prefix = "") { # nolint
  paste0(prefix, paste(
    sample(
      c(0:9, letters, LETTERS),
      length, replace = TRUE),
    collapse = ""))
}

#' controlla se una stringa e' nell'altra
#'
#' @name .containsString
#' @rdname constainsString
#' @usage .containsString(stringa, substring)
#' @param stringa stringa da controllare
#' @param substring sottoscringa da cercare in `stringa`
#' @return -1 se non trova `substring` in `stringa` altrimenti la posizione
#' @export

.containsString <- function(stringa, substring) # nolint
  as.numeric(regexpr(substring, stringa)) != -1

#' Slides the \code{x} object in \code{n} parts
#'
#' @name slice
#' @export
#' @author Giuseppe Acito
#' @param x the object to be sliced
#' @param n the number of parts
#' @return the list containing the \code{n} parts

slice <- function(x, n) {
  if (n == 0) {
    x
  } else {
    num <- length(x)
    lapply(seq(1, num, n), function(i) x[i:min(i + n - 1, num)])
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
    assign(name, x[[name]], envir = parent.frame())
  }
}

#' returns \code{TRUE} if the current R sessions runs on a Windows system
#'
#' @name is.windows
#' @return \code{TRUE} if the current R sessions runs on a Windows system
#' @title Misc Utils
#' @export

is.windows <- function() Sys.info()[['sysname']] == "Windows" # nolint

#' Checks if system is OSX
#'
#' @name is.darwin
#' @title Misc Utils
#' @usage is.darwin()
#' @return `TRUE` if run on OSX, `FALSE` otherwise
#' @export

is.darwin <- function() Sys.info()[["sysname"]] == "Darwin" # nolint

#' internal function to check if the runtime environment is jenkins.
#'
#' more like an hack.
#'
#' @name is.jenkins
#' @usage is.jenkins()
#' @return `TRUE` if running under Jenkins, `FALSE` otherwise
#' @title Internal Functions
#' @export

is.jenkins <- function() Sys.getenv("BUILD_URL") != "" # nolint

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


#' ifelse come dio comanda
#'
#' Questo `ifelse0`, a differenze di `ifelse` non modifica
#' lo shape dell'oggetto ritornato. (il che, oggettivamente
#' l'e' una bischerata)
#'
#' @name ifelse
#' @usage ifelse(test, a, b)
#' @param test un predicato che deve restituire `TRUE/FALSE`
#' @param a l'oggetto ritornato se `test` e' `TRUE`
#' @param b l'oggetto ritornato se `test` e' `FALSE`
#' @export

ifelse <- function(test, a, b) {
  if (test) a else b
}

#' checks for multicore environment
#'
#' @export

check_multi_core <- function() {
  # check sul numero di cores utilizzati
  ln <- "rutils::check_multi_core"
  tryCatch({
    if (foreach::getDoParWorkers() == 1) {
      .warn("Use doMC::registerDoMC(ncores) to exploit multi-core assets",
        name = ln)
    }
  }, error = function(cond) {
    .warn("Can't determine ncores, assuming 1 core (Root: %s)", cond,
      name = ln)
  })
}