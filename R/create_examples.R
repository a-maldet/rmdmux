#' Copy files for example-1 to directory
#' 
#' @param path An optional character string, holding the relative path to
#'   to the directory, where the example folder should be copied to.
#'   The default value is the current working directory.
#' @export
create_example_1 <- function(path = ".") {
  err_h <- function(msg)
    stop(paste("Error while calling `create_example_1()`:", msg), call. = FALSE)
  create_example_helper(path, "ex1", err_h = err_h)
}

create_example_helper <- function(path, dir_ex, err_h) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !dir.exists(path))
    err_h("Argument `path` must be a string containing a valid path to an existing directory")
  ex_files <- list.files(
    system.file("examples", dir_ex, package = "rmdmux"),
    full.names = TRUE
  )
  if (isTRUE(system.file("examples", dir_ex, package = "rmdmux") == ""))
    err_h(paste0("Example `", dir_ex, "` could not be found in installed 'rmdmux' package."))
  tryCatch(
    {
      file.copy(ex_files, path, overwrite = TRUE)
      run_file <- list.files(path, pattern = ".*_run\\.R", full.names = TRUE)[1]
      if (file.exists(run_file))
        writeLines(
          c(
            paste0("setwd('", normalizePath(path, winslash = "/"), "')"),
            "",
            readLines(con = run_file)
          ),
          con = run_file
        )
    },
    error = function(e) {
      err_h(paste0("Could not copy the example files to\n  `", path, "`\n", e))
    }
  )
}