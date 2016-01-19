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
#' @importFrom parallel makeForkCluster clusterExport detectCores stopCluster parLapply
#' @importFrom foreach foreach
#' @importFrom doParallel registerDoParallel
#' @exportClass Cluster
#' @export Cluster

Cluster <- setRefClass(  
  "Cluster",
  fields = c("ncores", "cluster", "working", "active"),
  methods = list(
    initialize = function(ncores=detectCores()) {
      "Initializes the Cluster by the number of cores argument (ncores)"
      .self$start(ncores)
    },
    
    export = function(ids, envir=parent.frame()) {
      "Exports the objects identified by `ids` to the cluster"
      clusterExport(
        .self$cluster,
        ids,
        envir=envir)
    },


    evalQ = function(...) {
        clusterEvalQ(.self$cluster, ...)
    },
    
    isWorking = function() {
      "Returns `TRUE` is the cluster is in working state"
      .self$working
    },

    start = function(ncores) {
      currentCluster <- getOption("CLUSTER", NULL)
      .self$working <- FALSE
      if(!is.null(currentCluster)) {
        .self$cluster <- currentCluster
      } else {
        .self$ncores <- ncores
        .self$cluster <- makeForkCluster(ncores)
        .self$active <- TRUE
        setDefaultCluster(.self$cluster)
        registerDoParallel(.self$cluster)
      }
      options(CLUSTER=.self$cluster)
    },
    
    shutdown = function() {
      if(.self$active) {
        tryCatch(
          stopCluster(.self$cluster),
          error = function(cond) {
            warning(cond)
          })
        options(CLUSTER=NULL)
        .self$active <- FALSE
      } else {
        warning("Cluster already closed")
      }
    },
    
    submit = function(X, fun, ...) {
      "Submit a `fun` to the cluster to operate on the `X` data"
      if(.self$working) {
        .self$shutdown()
        .self$initialize(.self$ncores)
      }
      
      .self$working <- TRUE
      ret <- parLapply(.self$cluster, X, fun, ...)
      .self$working <- FALSE
      ret
    },

    foreach = function(..., .combine, FUN) {
        foreach::foreach(..., .combine) %dopar% FUN
    }
  ))
