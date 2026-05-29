source("scripts/migrate-posts.R")

front_matter <- extract_front_matter(c(
  "---",
  "title: Test",
  "date: '2025-05-15'",
  "---",
  "Body"
))

stopifnot(identical(
  slugify_post("2025 SuperNationals VIII: A Once-in-Four-Years Chess Spectacle"),
  "2025-supernationals-viii-a-once-in-four-years-chess-spectacle"
))
stopifnot(identical(front_matter$body, "Body"))
stopifnot(identical(old_hugo_url("2025-05-15", "example-post"), "/post/2025/05/15/example-post/"))
stopifnot(identical(
  convert_postref('<img src="{{< blogdown/postref >}}index_files/a.png" />'),
  '<img src="index_files/a.png" />'
))
