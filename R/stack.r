
#' @export
#' @importFrom R6 R6Class

Stack <- R6Class(
  "Stack",
  public = list(
    initialize = function(...) {
      for(item in list(...)) {
        self$push(item)
      }
    },
    
    push = function(item) {
      private$stack <- append(item, private$stack)
    },

    peek = function() {
      stopifnot(!self$empty())
      private$stack[[1]]
    },

    pop = function() {
      stopifnot(!self$empty())
      ret <- private$stack[[1]]
      private$stack <- tail(private$stack, self$length() - 1)
      ret
    },
    
    empty = function() self$length() == 0,
    
    length = function() length(private$stack),

    show = function() {
      if (self$empty()) {
        cat("Empty Stack\n")
        return()
      }
      for (item in private$stack) {
        cat(item)
        cat("\n")
      }
    }
  ),

  private = list(
    stack = list()
  ))



#' Returns `TRUE` if `x` is a Stack
#'
#' @name is.Stack
#' @usage is.Stack(x)
#' @param x object to check
#' @return `TRUE` if x is a Stack; `FALSE` otherwise
#' @export

is.Stack <- function(x) {
  inherits(x, "Stack")
}
