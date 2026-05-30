# Yang Liu Research Blog

This repository contains the source for [liuyanguu.github.io](https://liuyanguu.github.io/), now built with [Quarto](https://quarto.org/) and R.

The previous Hugo/blogdown site is preserved under `hugo-xmag/` as a migration source and rollback reference. The active Quarto site lives at the repository root.

## Structure

- `_quarto.yml` configures the Quarto website.
- `index.qmd` is the homepage.
- `about.qmd` is the profile page.
- `posts/` contains migrated posts. Each post includes `index.qmd` plus the original `source.Rmd`.
- `post/` contains generated redirect pages for old Hugo URLs.
- `scripts/setup.R` checks the local R and Quarto setup.
- `scripts/migrate-posts.R` regenerates migrated posts from the old blogdown sources.
- `scripts/build.R` renders the site and publishes it to `C:/liuyanguu/liuyanguu.github.io`.

## Common Commands

Check the environment:

```r
source("scripts/setup.R")
```

Regenerate migrated posts from the old Hugo/blogdown sources. This overwrites polished Quarto post files, so it is guarded by an explicit environment variable:

```powershell
$env:QUARTO_MIGRATE_OVERWRITE = "true"
Rscript scripts/migrate-posts.R
Remove-Item Env:QUARTO_MIGRATE_OVERWRITE
```

Preview locally:

```bash
quarto preview
```

Render the site:

```bash
quarto render
```

Render and copy the generated site into the GitHub Pages repository:

```bash
Rscript scripts/build.R
```

## Authoring

New posts should be written as Quarto/R Markdown documents with R code chunks. Migrated posts preserve the original rendered output for reliability and keep the original R Markdown source beside each post as `source.Rmd`.
