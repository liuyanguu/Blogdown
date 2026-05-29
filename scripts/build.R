run <- function(command, args = character(), workdir = getwd()) {
  message("$ ", paste(c(command, args), collapse = " "))
  old <- setwd(workdir)
  on.exit(setwd(old), add = TRUE)
  status <- system2(command, args, stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed with status %s: %s", status, paste(c(command, args), collapse = " ")), call. = FALSE)
  }
}

copy_tree <- function(from, to) {
  from <- normalizePath(from, winslash = "/", mustWork = TRUE)
  to <- normalizePath(to, winslash = "/", mustWork = TRUE)

  expected <- normalizePath("C:/liuyanguu/liuyanguu.github.io", winslash = "/", mustWork = TRUE)
  if (!identical(to, expected)) {
    stop("Refusing to publish outside C:/liuyanguu/liuyanguu.github.io", call. = FALSE)
  }
  if (!dir.exists(file.path(to, ".git"))) {
    stop("Refusing to publish: destination does not look like the GitHub Pages repo", call. = FALSE)
  }

  protected <- c(".git", ".gitignore", "liuyanguu.github.io.Rproj")
  existing <- list.files(to, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  remove <- existing[!basename(existing) %in% protected]
  if (length(remove)) {
    unlink(remove, recursive = TRUE, force = TRUE)
  }

  generated <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  ok <- file.copy(generated, to, recursive = TRUE, copy.date = TRUE, overwrite = TRUE)
  if (any(!ok)) {
    stop("Some generated files could not be copied to the deploy repo.", call. = FALSE)
  }
}

run("quarto", "render")
source("scripts/clean-output.R")
copy_tree("_site", "C:/liuyanguu/liuyanguu.github.io")
message("Rendered and published Quarto site to C:/liuyanguu/liuyanguu.github.io")
