test_that(".info calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  info_mock <- mockery::mock()
  mockery::stub(.info, "futile.logger::flog.info", info_mock)
  expect_error(.info("ciao mondo"), NA)
  mockery::expect_called(info_mock, 1)
})

test_that(".debug calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  debug_mock <- mockery::mock()
  mockery::stub(.debug, "futile.logger::flog.debug", debug_mock)
  expect_error(.debug("ciao mondo"), NA)
  mockery::expect_called(debug_mock, 1)
})

test_that(".warn calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  warn_mock <- mockery::mock()
  mockery::stub(.warn, "futile.logger::flog.warn", warn_mock)
  expect_error(.warn("ciao mondo"), NA)
  mockery::expect_called(warn_mock, 1)
})

test_that(".error calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  error_mock <- mockery::mock()
  mockery::stub(.error, "futile.logger::flog.error", error_mock)
  expect_error(.error("ciao mondo"), NA)
  mockery::expect_called(error_mock, 1)
})


test_that(".trace calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  trace_mock <- mockery::mock()
  mockery::stub(.trace, "futile.logger::flog.trace", trace_mock)
  expect_error(.trace("ciao mondo"), NA)
  mockery::expect_called(trace_mock, 1)
})


test_that(".fatal calls futile.logger", {
  skip_if_not(requireNamespace("mockery"))

  fatal_mock <- mockery::mock()
  mockery::stub(.fatal, "futile.logger::flog.fatal", fatal_mock)
  expect_error(.fatal("ciao mondo"), NA)
  mockery::expect_called(fatal_mock, 1)
})


test_that("should_roll_file returns false if file doesn't exist", {
  skip_if_not(requireNamespace("mockery"))
  file_exist_mock <- mockery::mock(FALSE)
  mockery::stub(should_roll_file, "file.exists", file_exist_mock)
  expect_false(should_roll_file("/ciccio", 1000))
  mockery::expect_called(file_exist_mock, 1)
})

test_that("should_roll_file returns false if file size isn't enough", {
  skip_if_not(requireNamespace("mockery"))

  file_exist_mock <- mockery::mock(TRUE)
  file_info_mock <- mockery::mock(list(size = 999))
  mockery::stub(should_roll_file, "file.exists", file_exist_mock)
  mockery::stub(should_roll_file, "file.info", file_info_mock)

  expect_false(should_roll_file("/ciccio", 1000))
  mockery::expect_called(file_exist_mock, 1)
  mockery::expect_called(file_info_mock, 1)
})

test_that("should_roll_file is true if file exists and larger than max_size", {
  skip_if_not(requireNamespace("mockery"))

  file_exist_mock <- mockery::mock(TRUE)
  file_info_mock <- mockery::mock(list(size = 1001))
  mockery::stub(should_roll_file, "file.exists", file_exist_mock)
  mockery::stub(should_roll_file, "file.info", file_info_mock)

  expect_true(should_roll_file("/ciccio", 1000))
  mockery::expect_called(file_exist_mock, 1)
  mockery::expect_called(file_info_mock, 1)
})

test_that("init logging fails if the config file doesn't exist", {
  skip_if_not(requireNamespace("mockery"))
  file_exist_mock <- mockery::mock(FALSE)
  expect_error(init_logging("/notexist"))
})

test_that("appender_rolling's function raises an error if not called by futile.logger", { # nolint  
  workdir <- tempdir()
  on.exit({
    fs::dir_delete(workdir)
  })

  logfile <- file.path(workdir, "file.log")

  appender <- appender_rolling(logfile, max_files=2, max_size = 1000)
  expect_error(appender("helo world!"), "Illegal function call")
})

test_that("rolling file creates a new file when the filelength > max_size", {
  workdir <- tempdir()
  on.exit({
    # fs::dir_delete(workdir)
  })

  max_size <- 1000

  logfile <- file.path(workdir, "file.log")
  lockfile <- file.path(workdir, ".lock")
  junk_content <- stringi::stri_rand_strings(1, max_size + 1)
  cat(junk_content, file = logfile, sep = "", append = TRUE)
  expect_true(should_roll_file(logfile, max_size))

  rolling_file(logfile, max_size, 2, lockfile)
  glob <- Sys.glob(paste0(logfile, "*"))
  
  expect_equal(length(glob), 1)
  expect_equal(glob[[1]], paste0(logfile, ".1"))
})

test_that("rolling file creates max_files", {
  workdir <- tempdir()
  on.exit({
    fs::dir_delete(workdir)
  })

  max_size <- 1000
  max_files <- 5
  logfile <- file.path(workdir, "file.log")
  lockfile <- file.path(workdir, ".lock")
  junk_content <- stringi::stri_rand_strings(1, max_size + 1)

  for (i in seq(1, 10)) {
    cat(junk_content, file = logfile, sep = "", append = TRUE)
    expect_true(should_roll_file(logfile, max_size))
    rolling_file(logfile, max_size, max_files, lockfile)
  }

  glob <- Sys.glob(paste0(logfile, "*"))
  expect_equal(length(glob), max_files)
  for (index in seq(1, max_files)) {
    expect_equal(glob[[index]], paste0(logfile, ".", index))
  }
})

test_that("serious_layout_colored prints the userid", {
  level <- "INFO"
  names(level) <- level

  line <- serious_layout_colored(level, "hello world!")
  expect_match(line, whoami())
  expect_no_match(line, "junk")
})

test_that("serious_layout_colored prints the PID", {
  level <- "INFO"
  names(level) <- level

  line <- serious_layout_colored(level, "hello world!")
  expect_match(line, as.character(Sys.getpid()))
})

test_that("serious_layout_colored contains the level as string", {
  level <- "INFO"
  names(level) <- level

  line <- serious_layout_colored(level, "hello world!")
  expect_match(line, level)
  for(level in c("DEBUG", "TRACE", "INFO", "WARN", "ERROR", "FATAL")) {
    the_level <- level
    names(the_level) <- level
    line <- serious_layout_colored(the_level, "hello world!")
    expect_match(line, level)
  }
})