#' Stack data structure
#'
#' This is a stack data structure, operations you can do with it are:
#'
#'  - push
#'  - pop
#'  - peek
#'
#' @export
#' @examples \dontrun{
#'  s <- Stack()
#'  s$push(1)
#'  s$peek()
#'  s$pop()
#' }

Stack <- R6::R6Class( # nolint
  "Stack",
  public = list(
    #' Constructor for Stack
    #' Every item passed in the ellipsis is pushed in the stack
    #' @param ... items to be pushed in the stack
    initialize = function(...) {
      for (item in list(...)) {
        self$push(item)
      }
    },

    #' Push an item in the stack
    #'
    #' @param item object to be pushed
    push = function(item) {
      private$stack <- append(item, private$stack)
    },

    #' Peek an item from the stack
    #'
    #' Return the item on top of the stack. Raises an error if the
    #' stack is empty
    #'
    #' @return element on top of the stack
    peek = function() {
      stopifnot(!self$empty())
      private$stack[[1]]
    },

    #' Pops an item from the stack
    #'
    #' returns and removes the item on top of the stack. If the stack
    #' is empty, it raises an error.
    #'
    #' @return element on top of the stack
    pop = function() {
      stopifnot(!self$empty())
      ret <- private$stack[[1]]
      private$stack <- tail(private$stack, self$length() - 1)
      ret
    },

    #' `TRUE` is stack is empty
    #'
    #' @return `TRUE` if empty, `FALSE` otherwise
    empty = function() self$length() == 0,

    #' Returns the length of the stack
    #'
    #' @return the number of elements in the stack
    length = function() length(private$stack),

    #' Internal method for object rappresentatio on REPL
    #'
    #' @param sep separator used on the output
    show = function(sep="\n") {
      if (self$empty()) {
        cat("Empty Stack\n")
        return(invisible(NULL))
      }
      cat(paste0(private$stack, collapse = sep))
      invisible(NULL)
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

is.Stack <- function(x) { # nolint
  inherits(x, "Stack")
}
