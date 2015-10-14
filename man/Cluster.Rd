\docType{class}
\name{Cluster}
\alias{Cluster}
\title{A Cluster proxy to internal cluster functions}
\description{
This proxy has been made to be less error prone and a
little easier to emply clusters in computations
}
\details{
no value added, just a small API
}
\section{Methods}{
\itemize{
\item \code{export(Class)}: Returns the result of coercing the object to
Class.  No effect on the object itself.

\item \code{initialize(ncores = detectCores())}: Initializes the Cluster by the number of cores argument (ncores)

\item \code{isWorking()}: Returns `TRUE` is the cluster is in working state

\item \code{submit(X, fun, ...)}: Submit a `fun` to the cluster to operate on the `X` data
}}

