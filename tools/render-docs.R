#!/usr/bin/env Rscript
# Convert the Word documents into HTML fragments that the app embeds.
#
# This has to happen at build time, not in the app: the deployed site runs R
# under WebAssembly, where pandoc does not exist. The generated .html files are
# committed so that a local runApp() works without pandoc, and CI regenerates
# them before exporting so the deployed copies never drift from the .docx.
#
# Usage:  Rscript tools/render-docs.R
# Add a document by dropping it in www/ and adding a row to `docs` below.

app_dir <- "UCS Satellite Database Interactive Visualization"

docs <- list(
  list(src = file.path(app_dir, "www", "User_Guide.docx"), out = file.path(app_dir, "user_guide.html")),
  list(src = file.path(app_dir, "www", "Report.docx"),     out = file.path(app_dir, "report.html"))
)

if (Sys.which("pandoc") == "") {
  stop("pandoc not found. Install it (brew install pandoc) or run this in CI.")
}

for (d in docs) {
  if (!file.exists(d$src)) {
    message("skip (not present): ", d$src)
    next
  }
  # Images are extracted alongside so figures in the report survive the
  # conversion; --embed-resources inlines them so a single file is enough.
  status <- system2("pandoc", c(
    shQuote(d$src),
    "--from", "docx",
    "--to", "html",
    "--embed-resources",
    "--wrap", "none",
    "-o", shQuote(d$out)
  ))
  if (status != 0) stop("pandoc failed on ", d$src)
  message("wrote ", d$out, " (", file.size(d$out), " bytes)")
}
