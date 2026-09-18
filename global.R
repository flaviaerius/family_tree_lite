suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(htmltools)
  library(base64enc)
})

# Uploaded photo folders can easily exceed Shiny's 5 MB default; raise the cap.
options(shiny.maxRequestSize = 60 * 1024^2)