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
per <- "2010_2015"
# output <- here::here("output_nceamm_pam", "ncea", per)
output <- "~/scratch/output_nceamm_pam/ncea/2010_2015/"
rcea::chk_create(output)

# Load files for analysis
load("FormatData_nceamm_pam/biotic.RData")
load("FormatData_nceamm_pam/species_sensitivity.RData")
load("FormatData_nceamm_pam/metaweb.RData")
load("FormatData_nceamm_pam/TrophicSensitivity.RData")
load("FormatData_nceamm_pam/driversRaster.RData")
drivers <- drivers[[per]]

# Network-scale cumulative effects assessment
# focus <- "uria.137041"
# i <- which(names(biotic) == focus)

i <- as.numeric(commandArgs(trailingOnly = TRUE))
rcea::ncea_species(
  focus = names(biotic)[i],
  drivers, # [, 500:510, 300:310],
  biotic, # [, 500:510, 300:310],
  species_sensitivity,
  metaweb,
  trophic_sensitivity,
  w_d = 0.5,
  w_i = 0.25,
  output = output,
  output_format = "COG"
)
