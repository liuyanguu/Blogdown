site_dir <- normalizePath("_site", winslash = "/", mustWork = FALSE)
if (dir.exists(site_dir)) {
  unwanted <- file.path(site_dir, "hugo-xmag")
  if (dir.exists(unwanted)) {
    unlink(unwanted, recursive = TRUE, force = TRUE)
  }
}
