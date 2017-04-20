
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
    
    length = function() length(private$stack)
  ),

  private = list(
    stack = list()
  ))



is.Stack <- function(x) {
  inherits(x, "Stack")
}
