
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

test_that("ini_parse throws an error if file_name isn't an INI file", {
  skip_if_not(requireNamespace("mockery"))
  mockery::stub(ini_parse, "configr::is.ini.file", FALSE)
  fileini <- "/some/file.ini"
  expect_error(ini_parse(fileini))
})

test_that("ini_parse cannot parse multiple lines key-values", {
  fileini <- file.path(system.file(package = "rutils"), "ini/test.ini")
  ini <- expect_error(ini_parse(fileini), NA)
  expected <- paste("Very long on", "multiple lines", collapse = "\n")
  expect_false(ini$section3$verylong ==  expected)
})

test_that("toml_parse can parse multiple lines key-values", {
  file_toml <- file.path(system.file(package = "rutils"), "ini/test.toml")
  toml <- expect_error(toml_parse(file_toml), NA)
  expected <- paste("Very long on", "multiple lines", sep = "\\n")
  expect_equal(toml$section$verylong, expected)
})