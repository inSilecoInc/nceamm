#' Export figures
#'
#' @export
fig_cea_nceamm <- function() {
  out <- list()
  out$out <- here::here("figures")
  out$cea <- here::here(out$out, "cea_nceamm")
  lapply(out, chk_create)

  # Mask
  data(aoi)
  tmp <- stars::read_stars("data/drivers/transformed/2010_2015/fisheries_intensity.e2b7e6c4.DD.tif")
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi <- stars::st_rasterize(aoi["val_ras"], template = tmp)

  # ---
  plotDat <- function(dat, out, suffix = "", type = "regular", main = NULL, sub = NULL) {
    nm <- tools::file_path_sans_ext(names(dat))
    png(
      here::here(out, glue::glue("{nm}{suffix}.png")),
      res = param$figures$resolution,
      width = param$figures$width,
      height = param$figures$height,
      units = "mm",
      pointsize = param$figures$pointsize
    )

    # Mask
    dat <- dat * aoi

    # Normalize
    dat <- dat / max(dat[[1]], na.rm = T)

    # Plot
    if (type == "regular") plot_nceadfo(dat, mainTitle = main, subTitle = sub)
    if (type == "dual") plot_nceadfo_dual(dat, mainTitle = main, subTitle = sub)
    dev.off()
  }

  # Species-scale and network-scale cumulative effects assessment - nceamm
  here::here("output", "cea_full", "ncea_2016_2021.tif") |>
    stars::read_stars() |>
    plotDat(out$cea, sub = "Cumulative effects score")
  file.rename(
    from = here::here(out$cea, "ncea_2016_2021.png"),
    to = here::here(out$cea, "nceamm.png")
  )

  # Species-scale and network-scale cumulative effects assessment - original
  here::here("output_original", "cea_full", "ncea_2016_2021.tif") |>
    stars::read_stars() |>
    plotDat(out$cea, sub = "Cumulative effects score")
  file.rename(
    from = here::here(out$cea, "ncea_2016_2021.png"),
    to = here::here(out$cea, "original.png")
  )

}
