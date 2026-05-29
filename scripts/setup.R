required_packages <- c(
  "knitr",
  "rmarkdown",
  "yaml",
  "jsonlite",
  "htmltools"
)

cat("R version:", R.version.string, "\n")
quarto_version <- tryCatch(system2("quarto", "--version", stdout = TRUE, stderr = TRUE), error = function(e) NA_character_)
cat("Quarto version:", paste(quarto_version, collapse = " "), "\n")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  cat("Missing required packages:\n")
  cat(paste0("  - ", missing_packages, collapse = "\n"), "\n")
  cat("Install them with:\n")
  cat(sprintf("install.packages(c(%s))\n", paste(sprintf('\"%s\"', missing_packages), collapse = ", ")))
  quit(status = 1)
}

cat("All required site-build R packages are installed.\n")
