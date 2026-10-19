# Deploy Literacy Gap Analyzer to shinyapps.io (permanent public HTTPS URL).
#
# One-time setup (on your computer with R):
#   1. Create a free account at https://www.shinyapps.io/
#   2. Account -> Tokens -> Show -> copy the rsconnect::setAccountInfo(...) line
#   3. Run that line once in R, then:
#        Rscript scripts/deploy_shinyapps.R
#
# Result URL shape:
#   https://<your-account>.shinyapps.io/literacy-gap-analyzer/

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

root <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."))
setwd(root)

# App entry is app.R in this folder; data/processed must ship with the bundle.
rsconnect::deployApp(
  appDir = root,
  appFiles = c(
    "app.R",
    "R",
    "data/processed",
    "README.md"
  ),
  appName = "literacy-gap-analyzer",
  appTitle = "Literacy Gap Analyzer",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
