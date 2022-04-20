#' Logs an INFO message
#'
#' @param ... params passed to the logger
#' @export
.info <- function(...) {
  futile.logger::flog.info(...)
}

#' Logs an DEBUG message
#'
#' @param ... params passed to the logger
#' @export
#' @note watchout: don't call this "debug": Baaad things will happen
.debug <- function(...) {
  futile.logger::flog.debug(...)
}

#' Logs an ERROR message
#'
#' @param ... params passed to the logger
#' @export
.error <- function(...) {
  futile.logger::flog.error(...)
}

#' Logs an WARN message
#'
#' @param ... params passed to the logger
#' @export
.warn <-  function(...) {
  futile.logger::flog.warn(...)
}

#' Logs an TRACE message
#'
#' @param ... params passed to the logger
#' @export
.trace <- function(...) {
  futile.logger::flog.trace(...)
}

#' Logs an FATAL message
#'
#' @param ... params passed to the logger
#' @export
.fatal <- function(...) {
  futile.logger::flog.fatal(...)
}


message_to_console <- function(line) {
  msg <- paste(line, collapse="")
  cond <- simpleMessage(msg)
  class(cond) <- c("futile.logger.message", class(cond))
  message(cond)
}

#' Logger appender with rolling file capabilities
#'
#' This extends the futile.logger::appender.file2 adding
#' rolling file capabilities
#'
#' @param format format of the file name
#' @param console output to console
#' @param inherit inherit levels from upper level loggers
#' @param max_size size in bytes before rolling (defaults 10Mb)
#' @param max_files number of files to keep (defaults 5)
#' @param lock_file lock file to prevent access to lock during rolling 
#'      operations. If `NULL` no locking is performed (at your own risk)
#' @export


appender_rolling <- function(filename, console = FALSE, inherit = TRUE,
                             max_size = 10 * 1024 * 1024, max_files = 5,
                             lock_file = NULL) {
  level_where <- -1 # ditto for the current "level"
  function(line) {
    if (console) message_to_console(line)

    err <- function(e) {
      stop("Illegal function call, must call from flog.trace,",
      "flog.debug, flog.info, flog.warn, flog.error, flog.fatal, etc.")
    }

    the_level <- tryCatch(get("level", envir = sys.frame(level_where)),
      error = err)
    the_threshold <- tryCatch(get("logger", envir = sys.frame(level_where)),
      error = err)$threshold

    if (inherit) {
      all_levels <- c(futile.logger::FATAL, futile.logger::ERROR,
        futile.logger::WARN, futile.logger::INFO, futile.logger::DEBUG,
        futile.logger::TRACE)
      levels <- names(all_levels[the_level <= all_levels & all_levels <= the_threshold]) # nolint
    } else {
      levels <- names(the_level)
    }

    # the_function <- .get.parent.func.name(func_where) # nolint
    the_pid <- Sys.getpid()
    filename <- gsub("~p", the_pid, filename, fixed = TRUE)

    # remove eventual crayon mess from line
    line <- crayon::strip_style(line)

    # rolling
    rolling_file(filename, max_size, max_files, lock_file)
    cat(line, file = filename, append = TRUE, sep = "")
    invisible()
  }
}

#' predicate that tells if a file needs to be "rolled"
#'
#' @param filename path to be checked
#' @param max_size size beyond that filename has to be rolled

should_roll_file <- function(filename, max_size) {
  file.exists(filename) && file.info(filename)$size > max_size
}

#' Rolls the file based on parameters
#'
#' Rolling of a file is based on the following conditions
#'  - file is bigger than a given size
#'  - number of rolling files are bigger than

rolling_file <- function(filename, max_size, max_files, lock_file) {
  lock <- filelock::lock(lock_file, exclusive = TRUE, timeout = Inf)
  on.exit(filelock::unlock(lock))

  if (!should_roll_file(filename, max_size)) return()

  # copy each max_file-1 to max_file from max_file back to 1
  for (index in seq(max_files - 1, 1)) {
    src_file <- glue::glue("{filename}.{index}")
    if (!file.exists(src_file)) next
    dst_file <- glue::glue("{filename}.{index + 1}")
    fs::file_move(src_file, dst_file)
  }

  fs::file_move(filename, glue::glue("{filename}.1"))
}

get_parent_func_name <- function(where_) {
  the_function <- tryCatch(deparse(sys.call(where_ - 1)[[1]]),
        error = function(e) "(shell)")
  the_function <- ifelse(
    length(grep("flog\\.", the_function)) == 0, the_function, "(shell)")

  the_function
}

string_to_loglevel <- function(str_level) {
  switch(
    str_level,
    "FATAL" = futile.logger::FATAL,
    "ERROR" = futile.logger::ERROR,
    "WARN"  = futile.logger::WARN,
    "INFO"  = futile.logger::INFO,
    "DEBUG" = futile.logger::DEBUG,
    "TRACE" = futile.logger::TRACE,
    futile.logger::INFO)
}


serious_layout_colored <- function(level, msg, id = "", ...) {
  call_level <- -11 # found empirically

  if (length(list(...)) > 0) {
    parsed <- lapply(list(...), function(x) if (is.null(x)) "NULL" else x)
    msg <- do.call(sprintf, c(msg, parsed))
  }

  color <- switch(
    names(level),
    "FATAL" = function(x) crayon::bgRed(crayon::black(x)),
    "ERROR" = crayon::red,
    "WARN"  = crayon::yellow,
    "INFO"  = crayon::blue,
    "DEBUG" = crayon::silver,
    "TRACE" = crayon::blurred,
    crayon::white)

  glue::glue("{level} [{time} - {user} - {pid} - {func_name}] {msg}{newline}",
    level = crayon::bold(color(names(level))),
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user = whoami(),
    pid = Sys.getpid(),
    func_name = get_parent_func_name(call_level),
    msg = msg,
    newline = crayon::reset("\n"))
}

# begin nolint
LOG_ROOT <- "ROOT"

LOG_APPENDER_DEF <- "appender"
LOG_FILE_DEF <- "file"
LOG_MAX_SIZE_DEF <- "max_size"
LOG_MAX_FILES_DEF <- "max_files"
LOG_CONSOLE_DEF <- "console"
LOG_LEVEL_DEF <- "level"
LOG_INHERIT_DEF <- "inherit"
LOG_LOCKFILE_DEF <- "lock_file"
# end nolint


appender_factory <- function(logger_def) {
  appender_fun <- eval(parse(text=logger_def[[LOG_APPENDER_DEF]]))

  appender_fun(
    logger_def[[LOG_FILE_DEF]], 
    console = as.logical(logger_def[[LOG_CONSOLE_DEF]]),
    inherit = as.logical(logger_def[[LOG_INHERIT_DEF]]),
    lock_file = logger_def[[LOG_LOCKFILE_DEF]])
}

#' Init logging based on configuration file
#'
#' This tries to mimic the common pattern to use
#' a single config file to configure loggers and appenders
#'
#' This is way simpler than any other logging library out there.
#'
#' @param config_filename path to the configuration file
#' @export

init_logging <- function(config_filename) {
  stopifnot(file.exists(config_filename))
  logger_names <- configr::eval.config.sections(config_filename)

  # default values for ROOT logger if not defined in
  # config_filename
  log_root_conf <- list()
  log_root_conf[[LOG_FILE_DEF]] <- "file.log"
  log_root_conf[[LOG_INHERIT_DEF]] <- TRUE
  log_root_conf[[LOG_MAX_SIZE_DEF]] <- 10 * 1024 * 1024
  log_root_conf[[LOG_MAX_FILES_DEF]] <- 5
  log_root_conf[[LOG_CONSOLE_DEF]] <- TRUE
  log_root_conf[[LOG_APPENDER_DEF]] <- "rutils::appender_rolling"
  log_root_conf[[LOG_LEVEL_DEF]] <- "TRACE"
  log_root_conf[[LOG_LOCKFILE_DEF]] <- file.path(
    path.expand("~"), ".rutils_logger.lock")

  for (log_name in logger_names) {
    logger_def <- configr::eval.config(
      config = log_name,
      file = config_filename)


    for (config_def in names(log_root_conf)) {
      logger_def[[config_def]] <- rutils::ifelse(
        config_def %in% names(logger_def),
        logger_def[[config_def]],
        log_root_conf[[config_def]])
    }

    # NOTE(giupo): this should handle different appenders, I dunno when
    #  but this should
    appender <- appender_factory(logger_def)

    futile.logger::flog.logger(
      log_name, string_to_loglevel(logger_def[[LOG_LEVEL_DEF]]),
      appender = appender,
      layout = serious_layout_colored)
  }
}