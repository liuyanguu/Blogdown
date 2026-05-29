# Quarto Research Blog Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build and render a modern Quarto version of the research blog while preserving all post content, R source files, generated figures, and old URL redirects.

**Architecture:** The repository root becomes the Quarto source site. A repeatable R migration script converts Hugo/blogdown posts into Quarto post folders, preserving rendered post bodies for reliable output and copying original `.Rmd` files as sidecar source. Quarto renders to `_site`, then the generated site is copied into `C:/liuyanguu/liuyanguu.github.io`.

**Tech Stack:** Quarto 1.9.36, R 4.5.3, base R migration scripts, knitr/rmarkdown for future R posts, CSS for the Research Lab visual direction, PowerShell/R verification commands.

---

### Task 1: Create Baseline Tests For Migration Helpers

**Files:**
- Create: `tests/test-migration.R`
- Later modify: `scripts/migrate-posts.R`

- [ ] **Step 1: Write the failing test**

Create `tests/test-migration.R` with base R assertions that source `scripts/migrate-posts.R` and verify these functions:

```r
source("scripts/migrate-posts.R")

stopifnot(identical(slugify_post("2025 SuperNationals VIII: A Once-in-Four-Years Chess Spectacle"), "2025-supernationals-viii-a-once-in-four-years-chess-spectacle"))
stopifnot(identical(extract_front_matter(c("---", "title: Test", "date: '2025-05-15'", "---", "Body"))$body, "Body"))
stopifnot(identical(old_hugo_url("2025-05-15", "example-post"), "/post/2025/05/15/example-post/"))
stopifnot(identical(convert_postref('<img src="{{< blogdown/postref >}}index_files/a.png" />'), '<img src="index_files/a.png" />'))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript tests/test-migration.R`

Expected: FAIL because `scripts/migrate-posts.R` does not exist.

### Task 2: Implement Migration Helpers And Site Scaffolding

**Files:**
- Create: `scripts/migrate-posts.R`
- Create: `scripts/build.R`
- Create: `scripts/setup.R`
- Create: `_quarto.yml`
- Create: `index.qmd`
- Create: `about.qmd`
- Create: `styles.css`
- Modify: `.gitignore`

- [ ] **Step 1: Implement migration helper functions**

Create `scripts/migrate-posts.R` with:

```r
slugify_post <- function(x) { ... }
extract_front_matter <- function(lines) { ... }
old_hugo_url <- function(date, slug) { ... }
convert_postref <- function(text) { ... }
```

The helpers must pass `tests/test-migration.R`.

- [ ] **Step 2: Run test to verify it passes**

Run: `Rscript tests/test-migration.R`

Expected: PASS with no output and exit code 0.

- [ ] **Step 3: Add Quarto root scaffold**

Create `_quarto.yml`, `index.qmd`, `about.qmd`, and `styles.css` matching the Research Lab design. Set Quarto output to `_site` and keep deployment copy in `scripts/build.R`.

- [ ] **Step 4: Add setup and build scripts**

Create `scripts/setup.R` to report R/Quarto versions and missing packages. Create `scripts/build.R` to render the site and copy `_site` contents into `C:/liuyanguu/liuyanguu.github.io` after validating both paths.

### Task 3: Migrate All Existing Posts

**Files:**
- Modify: `scripts/migrate-posts.R`
- Generate: `posts/*/index.qmd`
- Generate: `posts/*/source.Rmd`
- Generate: `posts/*/index_files/**`
- Generate: `redirects.json`

- [ ] **Step 1: Extend migration script to discover posts**

Discover all `.Rmd` files under `hugo-xmag/content/post`, including leaf bundles with `index.Rmd`. For each post, derive title, date, old slug, clean new slug, categories, tags, and rendered `.html` source when available.

- [ ] **Step 2: Generate Quarto post folders**

For every discovered post, create `posts/<clean-slug>/index.qmd`, copy the original `.Rmd` as `source.Rmd`, copy sibling `index_files`/asset folders, and copy related static post assets when referenced.

- [ ] **Step 3: Generate redirect metadata**

Create `redirects.json` mapping old Hugo URLs to new clean URLs for every post with a date and slug.

- [ ] **Step 4: Run migration**

Run: `Rscript scripts/migrate-posts.R`

Expected: all posts generated and redirect metadata created.

### Task 4: Finish Styling And Content Cleanup

**Files:**
- Modify: `styles.css`
- Modify: `index.qmd`
- Modify: `about.qmd`
- Modify: `README.md`
- Modify: `note.R`
- Modify: generated `posts/*/index.qmd` as needed

- [ ] **Step 1: Apply mechanical proofreading**

Run repo-wide cleanup for common wording in migrated content: `Github` to `GitHub`, `Blogdown` to `blogdown` when referring to the package, obvious malformed link/list text, and stale setup wording.

- [ ] **Step 2: Improve high-visibility copy**

Rewrite `README.md`, `about.qmd`, homepage intro, and generated post source-note wording so the site reads clearly.

- [ ] **Step 3: Keep note.R non-destructive**

Replace `note.R` with a short pointer to the new setup/build scripts.

### Task 5: Render And Debug

**Files:**
- Modify as required by errors found during render.
- Output: `_site/**`
- Output: `C:/liuyanguu/liuyanguu.github.io/**`

- [ ] **Step 1: Check Quarto installation**

Run: `quarto check`

Expected: Quarto reports usable installation.

- [ ] **Step 2: Render source site**

Run: `quarto render`

Expected: `_site` is generated without build-blocking errors.

- [ ] **Step 3: Publish generated files**

Run: `Rscript scripts/build.R`

Expected: `_site` renders and generated files are copied into `C:/liuyanguu/liuyanguu.github.io`.

- [ ] **Step 4: Debug systematically if any command fails**

Read the error, reproduce, identify root cause, make one focused fix, and rerun the failing command.

### Task 6: Browser And Link Verification

**Files:**
- Modify as required by verification failures.

- [ ] **Step 1: Serve the rendered site locally**

Run a local static server from `C:/liuyanguu/liuyanguu.github.io`.

- [ ] **Step 2: Verify pages in browser**

Open homepage, about page, posts listing, and representative posts at desktop and mobile widths.

- [ ] **Step 3: Verify redirects**

Open at least three old Hugo post URLs and confirm they redirect to new `/posts/<slug>/` pages.

- [ ] **Step 4: Search for broken local references**

Run a script or command to scan generated HTML for missing local assets.

### Task 7: Final Git Checkpoint

**Files:**
- Source repo: all Quarto migration changes.
- Deploy repo: rendered site changes.

- [ ] **Step 1: Run final verification**

Run: `Rscript tests/test-migration.R`, `quarto render`, and `Rscript scripts/build.R`.

- [ ] **Step 2: Review git status**

Run git status in both `C:/liuyanguu/Blogdown` and `C:/liuyanguu/liuyanguu.github.io`.

- [ ] **Step 3: Commit source changes**

Commit source changes in `C:/liuyanguu/Blogdown`.

- [ ] **Step 4: Commit rendered site changes**

Commit generated website changes in `C:/liuyanguu/liuyanguu.github.io` if the render is verified.
