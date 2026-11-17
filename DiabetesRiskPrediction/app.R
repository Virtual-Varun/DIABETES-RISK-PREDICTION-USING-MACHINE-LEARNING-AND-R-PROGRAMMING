# ============================================================
# app.R
# Diabetes Risk Prediction System - Plumber Application Server
# ============================================================

# Ensure working directory is correctly set to DiabetesRiskPrediction
current_dir <- normalizePath(getwd(), winslash = "/")
if (basename(current_dir) == "api") {
  current_dir <- dirname(current_dir)
}
if (dir.exists(file.path(current_dir, "DiabetesRiskPrediction"))) {
  current_dir <- file.path(current_dir, "DiabetesRiskPrediction")
}
setwd(current_dir)
app_root_dir <- current_dir

# Ensure user R package library is included if present
user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R/win-library/4.6")
if (dir.exists(user_lib) && !user_lib %in% .libPaths()) {
  .libPaths(c(user_lib, .libPaths()))
}

library(plumber)

# Ensure output directory exists for plots and reports
out_folder <- file.path(app_root_dir, "output")
if (!dir.exists(out_folder)) {
  dir.create(out_folder, recursive = TRUE)
}

# Pre-load HTML content at server initialization
index_html_path <- file.path(app_root_dir, "web", "index.html")
index_html_body <- if (file.exists(index_html_path)) {
  paste(readLines(index_html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  "<h1>Diabetes Risk Prediction Application</h1><p>index.html not found in web/</p>"
}

# Parse the API annotations
pr <- plumber::plumb(file.path(app_root_dir, "api", "plumber.R"))

# Reset working directory back to app root (plumber::plumb may change it to api/)
setwd(app_root_dir)

# Disable automatic Swagger docs to prevent forced /__docs__/ redirect loops
pr$setDocs(FALSE)

# Serve index.html directly when opening root URL "/"
pr$handle("GET", "/", function(req, res) {
  res$status <- 200
  res$setHeader("Content-Type", "text/html; charset=UTF-8")
  res$body <- index_html_body
  return(res)
})

# Helper function to serve static assets with correct MIME types
serve_static_file <- function(base_dir, sub_path, res) {
  file_path <- file.path(app_root_dir, base_dir, sub_path)
  if (!file.exists(file_path)) {
    res$status <- 404
    res$setHeader("Content-Type", "application/json")
    res$body <- jsonlite::toJSON(list(error = TRUE, message = paste("File not found:", file_path)), auto_unbox = TRUE)
    return(res)
  }
  
  ext <- tolower(tools::file_ext(sub_path))
  mime <- switch(ext,
    "css"  = "text/css; charset=UTF-8",
    "js"   = "application/javascript; charset=UTF-8",
    "json" = "application/json; charset=UTF-8",
    "png"  = "image/png",
    "jpg"  = "image/jpeg",
    "jpeg" = "image/jpeg",
    "svg"  = "image/svg+xml",
    "ico"  = "image/x-icon",
    "csv"  = "text/csv; charset=UTF-8",
    "pdf"  = "application/pdf",
    "application/octet-stream"
  )
  
  res$status <- 200
  res$setHeader("Content-Type", mime)
  file_size <- file.info(file_path)$size
  res$body <- readBin(file_path, "raw", n = file_size)
  return(res)
}

# Handle /web/ static assets
pr$handle("GET", "/web/<file:path>", function(req, res, file) {
  serve_static_file("web", file, res)
})

# Handle /output/ static assets (plots, CSVs)
pr$handle("GET", "/output/<file:path>", function(req, res, file) {
  serve_static_file("output", file, res)
})

# Also handle root /style.css and /script.js for direct references
pr$handle("GET", "/style.css", function(req, res) {
  serve_static_file("web", "style.css", res)
})

pr$handle("GET", "/script.js", function(req, res) {
  serve_static_file("web", "script.js", res)
})

# Start local server on port 8000
cat("============================================================\n")
cat("DIABETES RISK PREDICTION SYSTEM\n")
cat("Machine Learning & Clinical Analytics Platform\n")
cat("Working Directory:", getwd(), "\n")
cat("Server running on http://127.0.0.1:8000\n")
cat("Press Ctrl+C to stop the server\n")
cat("============================================================\n")

pr$run(host = "127.0.0.1", port = 8000)
