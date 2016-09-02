#!/usr/bin/env Rscript

library(testthat)
library(methods)
library(crayon)

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
#' http://ant.1045680.n5.nabble.com/schema-for-junit-xml-output-td1375274.html
#' http://windyroad.org/dl/Open%20Source/JUnit.xsd
#'
#' Output drawn from the SummaryReporter is gprinted to the standard
#' error stream during execution.
#'
#' @export
#' @exportClass JUnitReporter
#' @aliases JUnitReporter-class
#' @examples
#' if (require("XML")) {
#' test_package("testthat", reporter = JUnitReporter$new(file = "testjunit.xml"))
#' }
#' @keywords debugging

JUnitReporter <- setRefClass("JUnitReporter", contains = "Reporter",
  fields = list(
    "file" = "character",
    "results" = "list",
    "timer" = "ANY"),

  methods = list(
    initialize = function(file = "", ...) {
      if (!require("XML", quietly = TRUE)) {
        stop("Please install the XML package", call. = FALSE)
      }
      callSuper(...)
      file <<- file
      results <<- list()
    },

    start_reporter = function() {
      callSuper()
      results <<- list()
      timer <<- proc.time()
      context <<- "(ungrouped)"
    },

    start_context = function(desc) {
      callSuper(desc)
      cat(desc, ": ", file = stderr())
    },

    end_context = function() {
      callSuper()
      cat("\n", file = stderr())
    },

    add_result = function(result) {
      if (result$passed) {
        cat(colourise(".", fg = "light green"), file = stderr())
      } else {
        failed <<- TRUE
        if (result$error) {
          cat(colourise("F", fg = "red"), file = stderr())
        } else {
          cat(colourise("E", fg = "red"), file = stderr())
        }
      }
      result$time <- round((proc.time() - timer)[["elapsed"]], 2)
      timer <<- proc.time()
      result$test <- if (is.null(test) || test == "") "(unnamed)" else test
      result$call <- if (is.null(result$call)) "(unexpected)" else result$call
      results[[context]] <<- append(results[[context]], list(result))
    },

    end_reporter = function() {
      cat("\n", file = stderr())
      xmlNodeOK <- function(name, ..., attrs = NULL) {
        ## do XML entity substitutions
        if (!is.null(attrs)) {
          attrs <- sapply(attrs, function(x) toString(xmlTextNode(x)))
        }
        xmlNode(name, ..., attrs = attrs)
      }
      classnameOK <- function(text) {
        gsub("[ \\.]", "_", text)
      }
      suites <- lapply(names(results), function(context) {
        x <- results[[context]]
        xnames <- vapply(x, "[[", "call", FUN.VALUE = character(1))
        xnames <- make.unique(xnames, sep = "_")
        for (i in seq_along(x)) {
          x[[i]]$call <- xnames[[i]]
        }
        testcases <- lapply(x, function(result) {
          failnode <- NULL
          if (!result$passed) {
            failnode <-
              xmlNodeOK("failure", attrs =
                        c(type = ifelse(result$error, "error", "failure"),
                          message = gsub("\n.*", "", result$message)),
                        .children = list(xmlTextNode(result$message)))
          }
          xmlNodeOK("testcase", attrs =
                    c(classname = paste(classnameOK(context),
                          classnameOK(result$test), sep = "."),
                      name = result$call,
                      time = result$time),
                    .children = if (!result$passed) list(failnode))
        })
        ispass <- sapply(x, "[[", "passed")
        iserr <- sapply(x, "[[", "error")
        tests <- sapply(x, "[[", "test")
        xmlNodeOK("testsuite", attrs =
                  c(tests = length(x),
                    failures = sum(!ispass & !iserr),
                    errors = sum(iserr),
                    name = context,
                    time = sum(sapply(x, "[[", "time")),
                    timestamp = toString(Sys.time()),
                    hostname = Sys.info()[["nodename"]]),
                  .children = testcases)
      })
      cat(toString(xmlNode("testsuites", .children = suites)),
          file = file)
    }
  )
)

JunitReporter <- R6::R6Class(
  "JunitReporter", inherit = Reporter,
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
        result <- list <- self$results[[context]]
        xnames <- vapply(result_list, `[[`, character(1), "call")
        xnames <- make.unique(xnames, sep = "_")
        for (i in seq_along(result_list)) {
          result <- list[[i]]$call <- xnames[[i]]
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
              xml <- new_node("failure", attrs =
                                           c(type = ifelse(result$error, "error", "failure"),
                                             message = location),
                              text = as.character(result))
          }
          xml <- new_node("testcase", attrs =
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
        xml <- new_node("testsuite", attrs =
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
      lapply(suites, function(suite)xml <- add_child(xmlDoc, suite))
      
      # this causes a segfault write_xml(xmlDoc, self$file, format = )
      cat(toString(xmlDoc), file = self$file)
      
    } # end_reporter
  )
)

if(require(xml2)) {
  file.xml <- file.path(
    Sys.getenv("WORKSPACE", "."), "tests",
    paste0(Sys.getenv("BUILD_NUMBER", "/tests"), ".xml"))
  print (file.xml)
  reporter <- JUnitReporter$new(file.xml)
} else {
  reporter = "summary"
}

test_check("rcf", reporter=reporter)
