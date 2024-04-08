#' Species-specific sensitivity data to use for assessment
#'
#' @export
make_species_sensitivity <- function() {
  # Load trophic sensitivity data
  sensitivity <- vroom::vroom(
    here::here(
      "data",
      "data-raw",
      "species_vulnerability-1386850b",
      "species_vulnerability-1386850b.csv"
    )
  )

  # Update for NCEAMM 2024
  # Adding vulnerability of marine mammals to fisheries
  # Get list of marine mammals
  input <- here::here("data", "data-integrated")
  sp <- vroom::vroom(
    here::here(input, "/species_list_marine_mammals_birds-7c150fc3/species_list_marine_mammals_birds-7c150fc3.csv")
  ) |>
    dplyr::filter(Class == "Mammalia")

  # Set sensitivity to 1 for the following fisheries:
  fish <- c(
    "FisheriesDD",
    "FisheriesDNH",
    "FisheriesPLB",
    "FisheriesPHB"
  )
  mm <- sensitivity$species %in% sp$ScientificName
  sensitivity[mm, fish] <- 1

  # Export
  out <- here::here("data", "cea_modules", "species_sensitivity")
  chk_create(out)
  write.csv(sensitivity, file = here::here(out, "species_sensitivity.csv"), row.names = FALSE)
}
