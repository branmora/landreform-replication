# Select actions ---------------------------------------------------------------
  
tables <- 0 # Clean data source 1
figures <- 0 # Clean data source 2
appendix_tables <- 0 # Constructs analysis datasets
appendix_figures <- 0 # Runs main analysis
  
# User path --------------------------------------------------------------------

github  <- file.path(" ")
data <- file.path(" ")

# Folder globals ---------------------------------------------------------------

scripts <- file.path(github, "scripts")
results <- file.path(github, "results")
  
# Packages used ----------------------------------------------------------------
  
packages <- c("tidyverse",
              "janitor",
              "haven",
              "modelsummary",
              "rdrobust")
  
# Load packages
invisible(sapply(packages, require, character.only = TRUE))

# Run code ------------------------------------------------------------------
if (tables) source(file.path(scripts, "01_tables.R"))

if (figures) source(file.path(scripts, "02_figures.R"))

if (appendix_tables) source(file.path(scripts, "03_appendix_tables.R"))
  
if (appendix_figures) source(file.path(scripts, "04_appendix_figures.R"))