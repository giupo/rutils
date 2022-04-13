test_that(".basename behaves returns the name without the ext",{
  file_name <- "/test/test/nomefile.txt"
  expect_equal(.basename(file_name), "nomefile")
  expect_true(.basename(file_name) != "nomefile.txt")
  expect_true(.basename(file_name) != basename(file_name))
})

test_that("random string generates a random string of the specified length", {
  rand1 <- .randomString()
  rand2_length <- 9
  rand2 <- .randomString(rand2_length)
  expect_true(rand1 != rand2)
  expect_equal(nchar(rand1), 8)
  expect_equal(nchar(rand2), rand2_length)
})

test_that("randomString generates a random string with a prefix", {
  rand1 <- .randomString(prefix = "ciaoMondo")
  expect_true(grepl("^ciaoMondo", rand1))
})

test_that("work_dir creates a directory", {
  file_path <- workDir()
  on.exit(unlink(file_path))
  infos <- file.info(file_path)
  expect_true(infos$isdir)
})


test_that("tempdir creates a directory", {
  file_path <- tempdir()
  infos <- file.info(file_path)
  expect_true(infos$isdir)
  unlink(file_path)
})

test_that("tempdir creates a directory", {
  file1_path <- tempdir()
  file2_path <- tempdir()
  on.exit({
    unlink(file1_path)
    unlink(file2_path)
  })
  expect_true(file1_path != file2_path)

})

test_that("containsString behaves as expected", {
  expect_true(.containsString("Ciao mondo", "iao"))
  expect_true(.containsString("AA", "BB") == FALSE)
  expect_true(.containsString("iao", "Ciao Mondo") == FALSE)
})

test_that("whoami returns a lowerified userid (used in BdI)", {
  expected <- c(letters, as.character(seq(0, 9)))
  x <- whoami()
  tokenize_whoami <- lapply(seq(1,nchar(x),1), function(i) substr(x, i, i))
  expect_true(all(tokenize_whoami %in% expected))
})


test_that("ini_parse works as expected", {
  fileini <- file.path(system.file(package = "rutils"), "ini/test.ini")
  ini <- ini_parse(fileini)
  expect_true("section" %in% names(ini))
  expect_true("section2" %in% names(ini))
  section <- ini$section
  expect_equal(section[["A"]], "B")
  expect_equal(section[["B"]], "1")

  section2 <- ini$section2
  expect_equal(section2[["A"]], "B")
  expect_equal(section2[["B"]], "A")
})

test_that("ini_parse throws an error with file_name in it", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(ini_parse, "configr::is.ini.file", FALSE)
  fileini <- "/some/file.ini"
  expect_error(ini_parse(fileini))
})

test_that("If there's username in the system env, return it", {
  skip_if_not(requireNamespace("mockery"))

  sys_getenv <- mockery::mock("pluto")
  mockery::stub(whoami, "Sys.getenv", sys_getenv)

  expect_equal(whoami(), "pluto")
  mockery::expect_called(sys_getenv, 1)
})

test_that("tempdir with prefix returns a dir with prefix", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(.containsString, "dir.create", function(...) {})
  tmp <- tempdir(prefix = "ciccio")
  expect_true(.containsString(tmp, "ciccio"))
})

test_that("unfold behaves as expected", {
  data <- list(a = 1, b = 2)
  unfold(data)
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("slice works as expected", {
  data <- list(a = 1, b = 1, c = 1, d = 1)
  expect_equal(slice(data, 0), data)
  expect_equal(slice(data, 2), list(list(a = 1, b = 1), list(c = 1, d = 1)))
})



test_that("rutils::ifelse doesn't changes the shape of objects", {
  x <- ts(c(1, 2, 3))
  y <- ts(c(3, 2, 1))

  expect_equal(ifelse(TRUE, x, y), x)
  expect_equal(ifelse(FALSE, x, y), y)

  expect_true(is.ts(ifelse(TRUE, x, y)))
  expect_true(is.ts(ifelse(FALSE, x, y)))

  expect_true(is.matrix(ifelse(FALSE, matrix(2, 2), matrix(2, 2))))
})


test_that("readLines fail showing the path", {
  expect_error(readLines("/NON/ESISTO/"), "/NON/ESISTO")
})

test_that("readLines fails not showing path if no file path is passed", {
  skip_if_not(requireNamespace("mockery"))
  base_readlines <- mockery::mock(stop("Urka"))
  mockery::stub(readLines, "base::readLines", base_readlines)
  expect_error(readLines(), "Urka")
  mockery::expect_called(base_readlines, 1)
})

test_that("is.darwin returns true if \"Darwin\" is sysname", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(is.darwin, "Sys.info", list(sysname = "Darwin"))
  expect_true(is.darwin())
})

test_that("is.windows returns true if \"Windows\" is sysname", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(is.windows, "Sys.info", list(sysname = "Windows"))
  expect_true(is.windows())
})

test_that("is.jenkins returns true if under jenkins", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(is.jenkins, "Sys.getenv", "NOT_EMPTY")
  expect_true(is.jenkins())
})

test_that("combine returns the cartesian product", {
  a <- c("A", "B")
  b <- c("1", "2", "3")
  x <- combine2(a, b)
  expect_equal(x, c("A1", "B1", "A2", "B2", "A3", "B3"))
})