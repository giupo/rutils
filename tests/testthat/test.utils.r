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
  tokenize_whoami <-  lapply(seq(1,nchar(x),1), function(i) substr(x, i, i))
  expect_true(all(tokenize_whoami %in% expected))
})

if(!is.jenkins()) {
  test_that("flypwd caches the pwd without calling the backend", {
    start <- as.numeric(Sys.time())
    flypwd()
    mid <- as.numeric(Sys.time())
    flypwd()
    end <- as.numeric(Sys.time())
    expect_true(end-mid <= mid-start, paste(end-mid, mid-start))
  })

  test_that("flypwd returns a single string and not a bunch of useless strings", {
    pwd <- flypwd()
    expect_equal(length(pwd), 1)
  })
}
