#!/usr/bin/env Rscript

library(testthat)
library(methods)
library(crayon)
library(R6)


#' @importFrom xml2 read_xml xml_attrs<- xml_add_child xml_text<-
xml_new_node <- function (name, attrs, children, text) {
  node <- read_xml(paste0("<", name, "/>"))
  xml_attrs(node) <- vapply(attrs, as.character, character(1))

  # add children
  if (!missing(children) && !is.null(children)) {
    if (inherits(children, 'xml_node'))
      children <- list(children)
    lapply(children, function(child)xml_add_child(node, child))
  }

  # set text
  if (!missing(text))
    xml_text(node) <- as.character(text)

  node
}


#' Test reporter: summary of errors in jUnit XML format.
#'
#' This reporter includes detailed results about each test and summaries,
#' written to a file (or stdout) in jUnit XML format. This can be read by
#' the Jenkins Continuous Integration System to report on a dashboard etc.
#' Requires the XML package.
#'
#' This works for \code{\link{expect_that}} but not for the wrappers like
#' \code{\link{expect_equal}} etc.
#'
#' To fit into the jUnit structure, context() becomes the \code{<testsuite>}
#' name as well as the base of the \code{<testcase> classname}. The
#' test_that() name becomes the rest of the \code{<testcase> classname}.
#' The deparsed expect_that() call becomes the \code{<testcase>} name.
#' On failure, the message goes into the \code{<failure>} node message
#' argument (first line only) and into its text content (full message).
#'
#' Execution time and some other details are also recorded.
#'
#' References for the jUnit XML format:
#' \url{http://llg.cubic.org/docs/junit/}
#'
#' Output drawn from the SummaryReporter is printed to the standard
#' error stream during execution.
#'
#' @export
#' @importFrom xml2 xml_new_document write_xml
JunitReporter <- R6::R6Class("JunitReporter", inherit = Reporter,
  public = list(
    file = NULL,
    results = NULL,
    timer = NULL,

    initialize = function(file = "report.xml") {
      super$initialize()
      self$file <- file
      self$results <- list()
    },

    start_reporter = function() {
      self$timer <- proc.time()
    },

    start_context = function(context) {
      self$cat(context, ": ")
    },

    end_context = function(context) {
      self$cat("\n")
    },

    add_result = function(context, test, result) {
      if (expectation_broken(result)) {
        self$cat_tight(single_letter_summary(result))
      }else {
        self$cat_tight(colourise(".", "success"))
      }

      result$time <- round((proc.time() - self$timer)[["elapsed"]], 2)
      self$timer  <- proc.time()
      result$test <- if (is.null(test) || test == "") "(unnamed)" else test
      # call can sometimes contain a second item, "succeed()"
      result$call <- if (is.null(result$call)) "(unexpected)" else format(result$call)[1]
      self$results[[context]] <- append(self$results[[context]], list(result))
    },

    end_reporter = function() {
      self$cat("\n")
      classnameOK <- function(text) {
        gsub("[ \\.]", "_", text)
      }
      # --- suites ---
      suites <- lapply(names(self$results), function(context) {
        result_list <- self$results[[context]]
        xnames <- vapply(result_list, `[[`, character(1), "call")
        xnames <- make.unique(xnames, sep = "_")
        for (i in seq_along(result_list)) {
          result_list[[i]]$call <- xnames[[i]]
        }
        testcases <- lapply(result_list, function(result) {
          failnode <- NULL
          if (expectation_broken(result)) {
            ref <- result$srcref
            if ( is.null(ref) ) {
              location <- ''
            } else {
              location <- paste0('(@', attr(ref, 'srcfile')$filename, '#', ref[1], ')')
            }
            failnode <-
              xml_new_node("failure", attrs =
                          c(type = ifelse(result$error, "error", "failure"),
                            message = location),
                          text = as.character(result))
          }
          xml_new_node("testcase", attrs =
                    c(classname = paste(classnameOK(context),
                                        classnameOK(result$test), sep = "."),
                      name = result$success_msg,
                      time = result$time,
                      message = result$success_msg),
                    children = if (expectation_broken(result)) list(failnode))
        }) # testcases
        ispass <- vapply(result_list, expectation_success, logical(1))
        iserr <- vapply(result_list, expectation_error,  logical(1))
        tests <- vapply(result_list, `[[`, character(1), "test")
        xml_new_node("testsuite", attrs =
                  c(tests = length(result_list),
                    failures = sum(!ispass & !iserr),
                    errors = sum(iserr),
                    name = context,
                    time = sum(vapply(result_list, `[[`, numeric(1), "time")),
                    timestamp = toString(Sys.time()),
                    hostname = Sys.info()[["nodename"]]),
                  children = testcases)
      }) # suites

      # create the final document
      xmlDoc <- xml_new_document()
      lapply(suites, function(suite)xml_add_child(xmlDoc, suite))

      # this causes a segfault write_xml(xmlDoc, self$file, format = )
      cat(toString(xmlDoc), file = self$file)

    } # end_reporter
  )
)

if(!is.null(Sys.getenv("WORKSPACE", NULL))) {
  file.xml <- file.path(
    Sys.getenv("WORKSPACE", "."), "tests",
    paste0(Sys.getenv("BUILD_NUMBER", "/tests"), ".xml"))
  print (file.xml)
  reporter <- JunitReporter$new(file=file.xml)
} else {
  reporter <- "summary"
}

test_check("rutils", reporter=reporter)