context("Generic Utils")

test_that(".basename behaves returns the name without the ext",{
  fileName <- "/test/test/nomefile.txt"
  expect_equal(.basename(fileName), "nomefile")
  expect_true(.basename(fileName) != "nomefile.txt")
  expect_true(.basename(fileName) != basename(fileName))
})

test_that("random string generates a random string of the specified length", {
  rand1 <- .randomString()
  rand2Length <- 9
  rand2 <- .randomString(rand2Length)
  expect_true(rand1 != rand2)
  expect_equal(nchar(rand1), 8)
  expect_equal(nchar(rand2), rand2Length)
})

test_that("randomString generates a random string with a prefix", {
  rand1 <- .randomString(prefix="ciaoMondo")
  expect_true(grepl("^ciaoMondo", rand1))
})

test_that("work_dir creates a directory", {
  file_path = workDir()
  infos <- file.info(file_path)
  expect_true(infos$isdir)
  unlink(file_path)
})

test_that("tempdir creates a directory", {
  file_path = tempdir()
  infos <- file.info(file_path)
  expect_true(infos$isdir)
  unlink(file_path)
})

test_that("tempdir creates a directory", {
  file1_path = tempdir()
  file2_path = tempdir()
  expect_true(file1_path != file2_path)
  unlink(file1_path)
  unlink(file2_path)
})

test_that("containsString behaves as expected", {
  expect_true(.containsString("Ciao mondo","iao"))
  expect_true(.containsString("AA", "BB") == FALSE)
  expect_true(.containsString("iao", "Ciao Mondo") == FALSE)
})

test_that("whoami returns a lowerified userid (used in BdI)", {
  expected <- c(letters, as.character(seq(0,9)))
  x <- whoami()
  tokenize_whoami <- lapply(seq(1,nchar(x),1), function(i) substr(x, i, i))
  expect_true(all(tokenize_whoami %in% expected))
})


test_that("flypwd caches the pwd without calling the backend", {
  expected <- "ioSonoLaLegge!"

  if(!require(mockery)) skip("mockery needed")

  stub(flypwd, 'system', function(...) expected)
  

  stub(flypwd, 'system', function(...) c("plainjunk", expected))
  x <- flypwd(clean=T)
  expect_equal(x, expected)
  

  stub(flypwd, 'system', function(...) "cambiata")
  expect_equal(flypwd(), expected)
  expect_equal(flypwd(clean=TRUE), "cambiata")

  
  ## damn side effects
  stub(flypwd, 'system', function(...) expected)
  expect_true(!flypwd() == expected)
  expect_equal(flypwd(clean=TRUE), expected)
})

test_that("flypwd: if i get a warn from system, flypwd raises an error", {
  # tolgo contenuti cache
  # expected <- "ioSonoLaLegge!"
  skip("non penso possiamo testartlo per come testthat gestisce i warning")
  # with_mock(
  #  'base::system' = function(...) { warning("ciao"); expected }, {
  #    flypwd(clean=T)
  #  }) 
})

test_that("flypwd returns a single string and not a bunch of useless strings", {
  if(!require(mockery)) skip("mockery needed") 
  expected <- "ioSonoLaLegge!"
  stub(flypwd, 'system', function(...) expected)
  pwd <- flypwd(clean=T)
  expect_equal(length(pwd), 1)
})

test_that("ini_parse works as expected", {
    fileini <- file.path(system.file(package="rutils"), "ini/test.ini")
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

test_that("ini_parse throws an error with filename in it", {
  fileini <- "/i/don/t/exist"
  expect_error(ini_parse(fileini), fileini)
})

test_that("If there's username in the system env, return it", {
  if(!require(mockery)) skip("mockery needed")

  stub(whoami, 'Sys.getenv', function(x, ... ) {
    if(x=="username") {
      "pluto"
    } else {
      base::Sys.getenv(x, ...)
    }
  })
  
  expect_equal(whoami(), "pluto")
})

test_that("tempdir with prefix returns a dir with prefix", {
  if(!require(mockery)) skip("mockery needed") 
  stub(.containsString, 'dir.create', function(...) {})
  tmp <- tempdir(prefix="ciccio")
  expect_true(.containsString(tmp, "ciccio"))
})

test_that("unfold behaves as expected", {
  data <- list(a=1,b=2)
  unfold(data)
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("slice works as expected", {
  data <- list(a=1,b=1,c=1,d=1)
  expect_equal(slice(data, 0), data)
  expect_equal(slice(data, 2), list(list(a=1,b=1), list(c=1,d=1)))
})
