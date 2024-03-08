#' Export figures
#'
#' @export
fig_whales <- function() {
  out <- list()
  out$out <- here::here("figures")
  out$whales <- here::here(out$out, "whales")
  out$ori <- here::here(out$whales, "orginal")
  out$pam <- here::here(out$whales, "pam")
  out$wsdb <- here::here(out$whales, "wsdb")
  out$pam_wsdb <- here::here(out$whales, "pam_wsdb")
  lapply(out, chk_create)

  # Mask
  data(aoi)
  tmp <- stars::read_stars("data/drivers/transformed/2010_2015/fisheries_intensity.e2b7e6c4.DD.tif")
  tmp[[1]][] <- NA
  aoi$val_ras <- 1
  aoi <- stars::st_rasterize(aoi["val_ras"], template = tmp)

  # ---
  plotTemplate <- function(dat, out, sub) {
    aoi <- sf::st_read("data/aoi/aoi.gpkg", quiet = TRUE)
    can <- sf::st_read("data/basemap/canada.gpkg", quiet = TRUE)
    usa <- sf::st_read("data/basemap/usa.gpkg", quiet = TRUE)

    # ------------------
    global_parameters()
    bbox <- param$bbox

    png(
      here::here(out),
      res = param$figures$resolution,
      width = param$figures$width,
      height = param$figures$height,
      units = "mm",
      pointsize = param$figures$pointsize
    )

    # ------------------
    par(family = "serif", mar = c(.5, .5, .5, .5))
    graphicsutils::plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
    image(dat, col = "#00000033")
    plot(sf::st_geometry(aoi), lwd = .5, border = param$col$aoi, add = TRUE)
    plot(sf::st_geometry(can), lwd = .5, col = param$col$coastline, add = TRUE)
    plot(sf::st_geometry(usa), lwd = .5, col = param$col$coastline, add = TRUE)

    pos <- par("usr")
    X <- mean(pos[1:2])
    Y <- mean(pos[3:4])
    text(x = X, y = Y, srt = 45, adj = .5, labels = "Not available", font = 2, cex = 1.5, col = "#00000099")

    plot_legend_bin(
      col = "#00000000",
      border = "#00000000",
      subTitle = sub
    )
    box()
    dev.off()
  }

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

    # Plot
    if (type == "regular") plot_nceadfo(dat, mainTitle = main, subTitle = sub)
    if (type == "dual") plot_nceadfo_dual(dat, mainTitle = main, subTitle = sub)
    dev.off()
  }

  get_names <- function(folder) {
    dir(folder) |>
      tools::file_path_sans_ext() |>
      stringr::str_split("-") |>
      lapply(function(x) x[[1]][1]) |>
      unlist()
  }

  # Whale species
  whales <- get_names("data/data-biotic/marine_mammals_pam_wsdb/binary/")
  whales_pam <- get_names("data/data-biotic/marine_mammals_pam/binary/")
  whales_ori <- get_names("data/data-biotic/marine_mammals/binary")

  # original
  glue::glue("output_original/ncea/2016_2021/net/{whales_ori}.tif") |>
    lapply(function(x) {
      dat <- stars::read_stars(x)
      tmp <- stars::st_apply(dat, c("x", "y"), sum, na.rm = TRUE)
      names(tmp) <- tools::file_path_sans_ext(names(dat))
      plotDat(tmp, out$ori, sub = "Orginal assessment")
    })

  # pam
  glue::glue("output/ncea/2016_2021/net/{whales_pam}.tif") |>
    lapply(function(x) {
      dat <- stars::read_stars(x)
      tmp <- stars::st_apply(dat, c("x", "y"), sum, na.rm = TRUE)
      names(tmp) <- tools::file_path_sans_ext(names(dat))
      plotDat(tmp, out$pam, sub = "Acoustic detections (PAM)")
    })

  # wsdb
  # WARNING: PUT BACK TO 2016-2021
  glue::glue("output_nceamm_wsdb/ncea/2010_2015/net/{whales}.tif") |>
    lapply(function(x) {
      dat <- stars::read_stars(x)
      tmp <- stars::st_apply(dat, c("x", "y"), sum, na.rm = TRUE)
      names(tmp) <- tools::file_path_sans_ext(names(dat))
      plotDat(tmp, out$wsdb, sub = "Whale sightings (WSDB)")
    })

  # pam_wsdb
  glue::glue("output_nceamm_pam_wsdb/ncea/2016_2021/net/{whales}.tif") |>
    lapply(function(x) {
      dat <- stars::read_stars(x)
      tmp <- stars::st_apply(dat, c("x", "y"), sum, na.rm = TRUE)
      names(tmp) <- tools::file_path_sans_ext(names(dat))
      plotDat(tmp, out$pam_wsdb, sub = "PAM & WSDB")
    })

  # Templates
  plotTemplate(aoi, here::here(out$whales, "tmp_pam.png"), sub = "Acoustic detections (PAM)")
  plotTemplate(aoi, here::here(out$whales, "tmp_ori.png"), sub = "Original assessment")

  # Combine figures with nceadfo results (if available)
  # Parameters
  hts <- 300
  img_resize <- "30%x30%"

  # Text position
  x1 <- 3250
  x2 <- 100
  y1 <- 300
  yG <- 2830
  t1_1 <- glue::glue("+{x1}+{y1}")
  t2_1 <- glue::glue("+{x2}+{y1}")
  t1_2 <- glue::glue("+{x1}+{y1 + yG}")
  t2_2 <- glue::glue("+{x2}+{y1 + yG}")

  # Functions
  nm_title <- function(img, chr) {
    magick::image_annotate(
      img,
      chr,
      location = "+75+100",
      size = 125,
      font = "Palatino",
      style = "Italic",
      weight = 1000,
      decoration = "underline",
      color = NULL,
    )
  }

  nm_sub <- function(img, chr) {
    magick::image_annotate(
      img,
      chr,
      location = "+75+250",
      size = 75,
      font = "Palatino",
      style = "Italic",
      weight = 200,
      color = "#494949",
    )
  }

  img_write <- function(img, path) {
    magick::image_write(
      img,
      path = path,
      format = "png",
      quality = NULL,
      depth = NULL,
      density = NULL,
      comment = NULL,
      flatten = FALSE,
      defines = NULL,
      compression = NULL
    )
  }

  chk_load <- function(path, template) {
    if (file.exists(path)) {
      magick::image_read(path)
    } else {
      magick::image_read(glue::glue("figures/whales/{template}.png"))
    }
  }

  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  # Create and get files
  nceadfo <- c() # TO FIGURE OUT
  pam <- dir(out$pam, full.names = TRUE)
  wsdb <- dir(out$wsdb, full.names = TRUE)
  pam_wsdb <- dir(out$pam_wsdb, full.names = TRUE)

  # Names
  nm <- whales |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_sentence()

  # Figures
  for (i in 1:length(nm)) {
    # Load images
    ## nceadfo # WARNING: CHANGE pam FOR ori
    i1 <- here::here(out$ori, glue::glue("{whales[i]}.png")) |>
      chk_load("tmp_ori")

    ## PAM
    i2 <- here::here(out$pam, glue::glue("{whales[i]}.png")) |>
      chk_load("tmp_pam")

    ## WSDB
    i3 <- here::here(out$wsdb, glue::glue("{whales[i]}.png")) |>
      chk_load("tmp_pam")

    ## PAM & WSDB
    i4 <- here::here(out$pam_wsdb, glue::glue("{whales[i]}.png")) |>
      magick::image_read()

    # Stack and combine images
    s1 <- magick::image_append(c(i1, i2))
    s2 <- magick::image_append(c(i3, i4))
    img <- magick::image_append(c(s1, s2), stack = TRUE)

    # Add border
    ht <- magick::image_info(img)$height
    img <- magick::image_border(img, glue::glue("0x{hts}"), color = "#ffffff") |>
      magick::image_crop(glue::glue("0x{ht+hts}"))

    # Add text
    img <- nm_title(img, nm[i])
    # img <- nm_sub(img, nm$aphiaID[i])

    # Resize
    img <- magick::image_resize(img, img_resize)

    # Export
    img_write(img, here::here(out$whales, glue::glue("{whales[i]}.png")))
    rm(img)
    gc()
  }
}
