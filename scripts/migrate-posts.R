suppressPackageStartupMessages({
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite_available <- TRUE
  } else {
    jsonlite_available <- FALSE
  }
  if (requireNamespace("yaml", quietly = TRUE)) {
    yaml_available <- TRUE
  } else {
    yaml_available <- FALSE
  }
})

slugify_post <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  x
}

strip_quotes <- function(x) {
  x <- trimws(x)
  x <- gsub("^['\"]|['\"]$", "", x)
  x
}

extract_front_matter <- function(lines) {
  if (length(lines) >= 2 && identical(trimws(lines[[1]]), "---")) {
    ends <- which(trimws(lines[-1]) == "---")
    if (length(ends) > 0) {
      end <- ends[[1]] + 1
      front <- if (end > 2) lines[2:(end - 1)] else character()
      body_lines <- if (length(lines) > end) lines[(end + 1):length(lines)] else character()
      return(list(front = front, body = paste(body_lines, collapse = "\n")))
    }
  }
  list(front = character(), body = paste(lines, collapse = "\n"))
}

parse_front_matter <- function(front) {
  if (yaml_available && length(front) > 0) {
    parsed <- tryCatch(yaml::yaml.load(paste(front, collapse = "\n")), error = function(e) NULL)
    if (is.list(parsed)) return(parsed)
  }

  out <- list()
  i <- 1
  while (i <= length(front)) {
    line <- front[[i]]
    match <- regexec("^([A-Za-z0-9_.-]+):\\s*(.*)$", line)
    parts <- regmatches(line, match)[[1]]
    if (length(parts) == 3) {
      key <- parts[[2]]
      value <- parts[[3]]
      if (identical(trimws(value), "")) {
        values <- character()
        j <- i + 1
        while (j <= length(front) && grepl("^\\s+-\\s+", front[[j]])) {
          values <- c(values, strip_quotes(sub("^\\s+-\\s+", "", front[[j]])))
          j <- j + 1
        }
        out[[key]] <- values
        i <- j
      } else {
        out[[key]] <- strip_quotes(value)
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }
  out
}

old_hugo_url <- function(date, slug) {
  date <- as.character(date)
  parts <- strsplit(date, "-", fixed = TRUE)[[1]]
  if (length(parts) < 3) return(NA_character_)
  sprintf("/post/%s/%s/%s/%s/", parts[[1]], parts[[2]], parts[[3]], slug)
}

convert_postref <- function(text) {
  text <- gsub("\\{\\{<\\s*blogdown/postref\\s*>\\}\\}", "", text, perl = TRUE)
  text <- gsub("\\{\\{<\\s*relref\\s+\"post/[^\"]+\"\\s*>\\}\\}", "", text, perl = TRUE)
  text <- gsub("\\{\\{<\\s*relref\\s+'post/[^']+'\\s*>\\}\\}", "", text, perl = TRUE)
  text
}

quote_yaml <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  sprintf("\"%s\"", x)
}

normalize_words <- function(x) {
  x <- gsub("Github", "GitHub", x, fixed = TRUE)
  x <- gsub("RMarkdown", "R Markdown", x, fixed = TRUE)
  x <- gsub("rmarkdown", "R Markdown", x, fixed = TRUE)
  x
}

html_description <- function(text, n = 220) {
  text <- gsub("<script\\b[^>]*>.*?</script>", " ", text, ignore.case = TRUE, perl = TRUE)
  text <- gsub("<style\\b[^>]*>.*?</style>", " ", text, ignore.case = TRUE, perl = TRUE)
  text <- gsub("<div id=\"TOC\".*?</div>", " ", text, ignore.case = TRUE, perl = TRUE)
  text <- gsub("<[^>]+>", " ", text, perl = TRUE)
  text <- gsub("&nbsp;", " ", text, fixed = TRUE)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- normalize_words(text)
  text <- trimws(gsub("\\s+", " ", text))
  if (nchar(text) > n) {
    text <- paste0(substr(text, 1, n - 1), "...")
  }
  text
}

yaml_vector <- function(key, values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(character())
  c(paste0(key, ":"), paste0("  - ", vapply(values, quote_yaml, character(1))))
}

safe_read_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

safe_write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
}

copy_dir_if_exists <- function(from, to) {
  if (!dir.exists(from)) return(FALSE)
  if (dir.exists(to)) unlink(to, recursive = TRUE, force = TRUE)
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, dirname(to), recursive = TRUE, copy.date = TRUE, overwrite = TRUE)
}

clean_body <- function(text, old_slug) {
  text <- convert_postref(text)
  text <- gsub('href="/rmarkdown-libs/', 'href="/rmarkdown-libs/', text, fixed = TRUE)
  text <- gsub('src="/rmarkdown-libs/', 'src="/rmarkdown-libs/', text, fixed = TRUE)
  text <- gsub(sprintf('src="/post/%s_files/', old_slug), sprintf('src="%s_files/', old_slug), text, fixed = TRUE)
  text <- gsub(sprintf('href="/post/%s_files/', old_slug), sprintf('href="%s_files/', old_slug), text, fixed = TRUE)
  shared_libs <- c(
    "header-attrs/header-attrs.js",
    "htmlwidgets/htmlwidgets.js",
    "viz/viz.js",
    "DiagrammeR-styles/styles.css",
    "grViz-binding/grViz.js"
  )
  for (lib in shared_libs) {
    text <- gsub(
      sprintf('src="%s_files/%s"', old_slug, lib),
      sprintf('src="../../rmarkdown-libs/%s"', lib),
      text,
      fixed = TRUE
    )
    text <- gsub(
      sprintf('href="%s_files/%s"', old_slug, lib),
      sprintf('href="../../rmarkdown-libs/%s"', lib),
      text,
      fixed = TRUE
    )
  }
  text <- gsub("Github", "GitHub", text, fixed = TRUE)
  text <- gsub("google blogger", "Google Blogger", text, fixed = TRUE)
  text
}

ensure_header_attrs_stub <- function() {
  path <- file.path("rmarkdown-libs", "header-attrs", "header-attrs.js")
  if (!file.exists(path)) {
    safe_write_lines(
      c(
        "/*",
        " * Compatibility stub for legacy R Markdown posts.",
        " * Quarto handles header attributes natively, so no runtime code is needed.",
        " */"
      ),
      path
    )
  }
}

discover_posts <- function(source_dir = "hugo-xmag/content/post") {
  rmds <- list.files(source_dir, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)
  rmds <- rmds[!grepl("index_files", rmds, fixed = TRUE)]
  rmds[order(rmds)]
}

post_record <- function(rmd_path) {
  rmd_lines <- safe_read_lines(rmd_path)
  rmd_fm <- extract_front_matter(rmd_lines)
  meta <- parse_front_matter(rmd_fm$front)

  file_base <- tools::file_path_sans_ext(basename(rmd_path))
  is_bundle <- identical(tolower(basename(rmd_path)), "index.rmd")
  path_slug <- if (is_bundle) basename(dirname(rmd_path)) else file_base
  file_slug_no_date <- sub("^\\d{4}-\\d{2}-\\d{2}-", "", path_slug)

  date <- as.character(meta$date %||% sub("^((\\d{4}-\\d{2}-\\d{2})).*$", "\\1", basename(rmd_path)))
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date) && grepl("^\\d{4}-\\d{2}-\\d{2}", basename(dirname(rmd_path)))) {
    date <- substr(basename(dirname(rmd_path)), 1, 10)
  }

  title <- normalize_words(as.character(meta$title %||% file_slug_no_date))
  author <- as.character(meta$author %||% "Yang Liu")
  slug <- as.character(meta$slug %||% file_slug_no_date)
  new_slug <- slugify_post(title)
  if (!nzchar(new_slug)) new_slug <- slugify_post(slug)

  rendered <- if (is_bundle) file.path(dirname(rmd_path), "index.html") else file.path(dirname(rmd_path), paste0(file_base, ".html"))
  body <- rmd_fm$body
  if (file.exists(rendered)) {
    html_fm <- extract_front_matter(safe_read_lines(rendered))
    body <- html_fm$body
  }

  categories <- meta$categories %||% character()
  tags <- meta$tags %||% character()
  if (!is.character(categories)) categories <- as.character(unlist(categories))
  if (!is.character(tags)) tags <- as.character(unlist(tags))
  categories <- normalize_words(categories)
  tags <- normalize_words(tags)

  list(
    rmd_path = rmd_path,
    rendered_path = rendered,
    source_dir = dirname(rmd_path),
    is_bundle = is_bundle,
    title = title,
    author = author,
    date = date,
    old_slug = slug,
    old_hugo_slug = path_slug,
    new_slug = new_slug,
    categories = categories,
    tags = tags,
    description = html_description(clean_body(body, path_slug)),
    body = clean_body(body, path_slug)
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

migrate_one_post <- function(record, target_root = "posts") {
  target_dir <- file.path(target_root, record$new_slug)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  file.copy(record$rmd_path, file.path(target_dir, "source.Rmd"), overwrite = TRUE, copy.date = TRUE)

  if (record$is_bundle) {
    for (asset_dir in c("index_files", "images", "temp")) {
      copy_dir_if_exists(file.path(record$source_dir, asset_dir), file.path(target_dir, asset_dir))
    }
    for (asset in list.files(record$source_dir, pattern = "\\.(png|jpg|jpeg|gif|svg|rds|csv|xlsx)$", ignore.case = TRUE, full.names = TRUE)) {
      if (!identical(normalizePath(asset, winslash = "/", mustWork = FALSE), normalizePath(record$rmd_path, winslash = "/", mustWork = FALSE)) &&
          !identical(normalizePath(asset, winslash = "/", mustWork = FALSE), normalizePath(record$rendered_path, winslash = "/", mustWork = FALSE))) {
        file.copy(asset, target_dir, overwrite = TRUE, copy.date = TRUE)
      }
    }
  }

  static_post_assets <- file.path("hugo-xmag/static/post", paste0(record$old_hugo_slug, "_files"))
  copy_dir_if_exists(static_post_assets, file.path(target_dir, paste0(record$old_hugo_slug, "_files")))

  static_root_assets <- file.path("hugo-xmag/static", paste0(record$old_hugo_slug, "_files"))
  copy_dir_if_exists(static_root_assets, file.path(target_dir, paste0(record$old_hugo_slug, "_files")))

  resources <- c(
    "source.Rmd",
    "index_files/**",
    "images/**",
    "temp/**",
    paste0(record$old_hugo_slug, "_files/**"),
    "*.png",
    "*.jpg",
    "*.jpeg",
    "*.JPG",
    "*.PNG",
    "*.gif",
    "*.svg",
    "*.rds",
    "*.csv",
    "*.xlsx"
  )

  header <- c(
    "---",
    paste0("title: ", quote_yaml(record$title)),
    paste0("author: ", quote_yaml(record$author)),
    paste0("date: ", quote_yaml(record$date)),
    paste0("description: ", quote_yaml(record$description)),
    yaml_vector("categories", record$categories),
    yaml_vector("tags", record$tags),
    "page-layout: article",
    "execute:",
    "  freeze: true",
    "  eval: false",
    "resources:",
    paste0("  - ", vapply(resources, quote_yaml, character(1))),
    "---",
    "",
    "::: {.callout-note appearance=\"simple\"}",
    "This migrated post preserves the original rendered output. The original R Markdown source is available as [source.Rmd](source.Rmd).",
    ":::",
    ""
  )

  safe_write_lines(c(header, record$body), file.path(target_dir, "index.qmd"))
  target_dir
}

write_redirect <- function(from_url, to_url) {
  parts <- strsplit(gsub("^/", "", from_url), "/", fixed = TRUE)[[1]]
  target <- do.call(file.path, as.list(c(parts, "index.qmd")))
  title <- paste("Redirect to", to_url)
  lines <- c(
    "---",
    paste0("title: ", quote_yaml(title)),
    "search: false",
    "page-layout: article",
    "format:",
    "  html:",
    "    include-in-header:",
    "      text: |",
    paste0("        <meta http-equiv=\"refresh\" content=\"0; url=", to_url, "\">"),
    "---",
    "",
    paste0("<script>window.location.replace(\"", to_url, "\");</script>"),
    "",
    paste0("This page has moved to [", to_url, "](", to_url, ").")
  )
  safe_write_lines(lines, target)
}

write_redirects_json <- function(redirects, path = "redirects.json") {
  if (jsonlite_available) {
    jsonlite::write_json(redirects, path, auto_unbox = TRUE, pretty = TRUE)
  } else {
    lines <- c("{", paste(sprintf('  "%s": "%s"', names(redirects), unname(redirects)), collapse = ",\n"), "}")
    safe_write_lines(lines, path)
  }
}

write_posts_index <- function(path = "posts/index.qmd") {
  lines <- c(
    "---",
    "title: \"Posts\"",
    "page-layout: full",
    "toc: false",
    "format:",
    "  html:",
    "    code-tools: false",
    "listing:",
    "  contents: \"*/index.qmd\"",
    "  sort: \"date desc\"",
    "  type: default",
    "  categories: true",
    "---",
    "",
    "All migrated research notes and technical posts."
  )
  safe_write_lines(lines, path)
}

migrate_posts <- function() {
  rmds <- discover_posts()
  if (!length(rmds)) stop("No Rmd posts found under hugo-xmag/content/post")

  if (dir.exists("posts")) unlink("posts", recursive = TRUE, force = TRUE)
  if (dir.exists("post")) unlink("post", recursive = TRUE, force = TRUE)
  dir.create("posts", recursive = TRUE, showWarnings = FALSE)

  records <- lapply(rmds, post_record)
  redirects <- list()

  for (record in records) {
    migrate_one_post(record)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", record$date) && nzchar(record$old_slug)) {
      from <- old_hugo_url(record$date, record$old_slug)
      to <- paste0("/posts/", record$new_slug, "/")
      redirects[[from]] <- to
      write_redirect(from, to)
    }
  }

  copy_dir_if_exists("hugo-xmag/static/rmarkdown-libs", "rmarkdown-libs")
  ensure_header_attrs_stub()
  dir.create("assets", showWarnings = FALSE)
  if (file.exists("hugo-xmag/static/me.png")) {
    file.copy("hugo-xmag/static/me.png", "assets/me.png", overwrite = TRUE, copy.date = TRUE)
  }

  write_posts_index()
  write_redirects_json(redirects)
  message(sprintf("Migrated %d posts and generated %d redirects.", length(records), length(redirects)))
}

if (sys.nframe() == 0 && !interactive()) {
  migrate_posts()
}
