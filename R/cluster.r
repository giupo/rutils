#' A Cluster proxy to internal cluster functions
#'
#' This proxy has been made to be less error prone and a little easier
#' to emply clusters in computations
#'
#' no value added, just a small API
#'
#' @name Cluster
#' @rdname Cluster
#' @field ncores stores the number of cores used for this cluster
#' @field cluster the internal cluster
#' @field working a logical status to fix some erratic behaviour of the internal cluster
#' @import parallel doParallel
#' @exportClass Cluster
#' @export

Cluster <- setRefClass(  
  "Cluster",
  fields = c("ncores", "cluster", "working"),
  methods = list(
    initialize = function(ncores=detectCores()) {
      "Initializes the Cluster by the number of cores argument (ncores)"
      currentCluster <- getOption("CLUSTER", NULL)
      if(!is.null(currentCluster)) {
        .self$cluster <- currentCluster
      } else {
        .self$ncores <- ncores
        .self$cluster <- makeForkCluster(ncores=.self$ncores)
      }      
    },
    
    isWorking = function() {
      "Returns `TRUE` is the cluster is in working state"
      .self$working
    },
    
    submit = function(X, fun, ...) {
      "Submit a `fun` to the cluster to operate on the `X` data"
      if(.self$working) {
        stopCluster(.self$cluster)
        options(CLUSTER=NULL)
        .self$initialize(.self$ncores)
      }
      
      .self$working <- TRUE
      ret <- parLapply(.self$cluster, X, fun, ...)
      .self$working <- FALSE
      ret
    } 
  ))
