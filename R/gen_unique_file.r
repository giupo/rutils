create_random_chars <- function(n, values = letters) {
  paste0(sample(values, n, TRUE), collapse = "")
}

gen_unique_filename <- function(n = 7, prefix = tempdir(), ext = NA) {
  dir.create(prefix, showWarnings = FALSE, recursive = TRUE)

  # lock file management
  lock_file <- file.path(prefix, ".lock")
  lock <- filelock::lock(lock_file)
  on.exit(filelock::unlock(lock))

  generate_file_name <- function(n, ext = NA) {
    random_name <- create_random_chars(n)

    filename <- if (is.na(ext)) random_name
    else glue::glue("{random_name}.{ext}")

    file.path(prefix, filename)
  }

  filename <- generate_file_name(n, ext = ext)
  while (file.exists(filename)) {
    filename <- generate_file_name(n, ext = ext)
  }

  return(filename)
}
