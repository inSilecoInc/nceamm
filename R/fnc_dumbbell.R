#' Highcharter dumbbell chart
#'
#' @param dat data.frame with columns `stressors` for stressor names, `direct` and `indirect` for direct and indirect effects of stressors on species of interest, respectively.
#'
#' @export

direct <- read.csv("output/ncea/2016_2021/cekm/balaenoptera_musculus_direct.csv") |>
  data.frame() |>
  dplyr::select(-vc) |>
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "stressors", values_to = "direct")

indirect <- read.csv("output/ncea/2016_2021/cekm/balaenoptera_musculus_indirect.csv") |>
  data.frame() |>
  dplyr::select(-vc) |>
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "stressors", values_to = "indirect")

dat <- dplyr::left_join(direct, indirect, by = "stressors")

dumbbell <- function(dat) {



}
