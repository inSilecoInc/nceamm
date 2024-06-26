# # install.packages(c(
# #   "devtools",
# #   "dplyr",
# #   "tidyr",
# #   "sf",
# #   "stars",
# #   "vroom",
# #   "here",
# #   "units",
# #   "terra"
# # ))
# # devtools::install_github("ecosystem-assessments/motifcensus")
# # devtools::install_github("ecosystem-assessments/rcea")
library(rcea)
library(dplyr)
library(tidyr)
library(stars)
library(terra)

# Specify and create output folder
per <- "2016_2021"
# output <- here::here("output", "ncea", per)
output <- "~/scratch/output/ncea/2016_2021/"
rcea::chk_create(output)

# Load files for analysis
load("FormatData/biotic.RData")
load("FormatData/species_sensitivity.RData")
load("FormatData/metaweb.RData")
load("FormatData/TrophicSensitivity.RData")
load("FormatData/driversRaster.RData")
drivers <- drivers[[per]]

# Network-scale cumulative effects assessment
# focus <- "uria.137041"
# i <- which(names(biotic) == focus)

i <- as.numeric(commandArgs(trailingOnly = TRUE))
rcea::ncea_species(
  focus = names(biotic)[i],
  drivers,
  biotic,
  species_sensitivity,
  metaweb,
  trophic_sensitivity,
  w_d = 0.5,
  w_i = 0.25,
  output = output,
  output_format = "COG"
)
