#' Download and aggregate SAFRAN-ISBA climate data over a catchment
#'
#' @description
#' Downloads daily SAFRAN-ISBA data from the GéoSAS EDR API for all grid cells
#' intersecting a catchment shapefile, then computes a weighted sum of all
#' meteorological parameters proportional to each cell's area within the catchment.
#'
#' @param ctm_path Character. Path to the catchment shapefile (.shp or any format readable by \code{sf}).
#' @param safran_path Character. Path to the SAFRAN grid GeoPackage (.gpkg). Default: \code{"data/safran.gpkg"}.
#' @param maille_path Character. Path to the SAFRAN cell centroids KML file. Default: \code{"data/SIM2.kml"}.
#' @param epsg Integer. EPSG code for the projection used during spatial intersection. Default: \code{2154} (Lambert 93).
#' @param show_map Logical. If \code{TRUE}, displays a map of the intersected SAFRAN cells and the catchment. Default: \code{FALSE}.
#'
#' @return A \code{data.table} with one row per day and one column per SAFRAN-ISBA
#'   parameter, containing the area-weighted aggregated values over the catchment.
#'
#' @examples
#' \dontrun{
#' df <- get_safran(
#'   ctm_path   = "data/bv_baget.shp",
#'   safran_path = "data/safran.gpkg",
#'   maille_path = "data/SIM2.kml"
#' )
#' }
#'
#' @importFrom sf read_sf st_transform st_crs st_intersection st_area st_bbox st_coordinates
#' @importFrom dplyr mutate
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_csv
#' @importFrom data.table setDT rbindlist
#' @importFrom ggplot2 ggplot geom_sf geom_sf_label aes theme_void
#'
#' @export
get_safran <- function(ctm_path,
                       safran_path = NULL,
                       maille_path = NULL,
                       epsg = 2154,
                       show_map = FALSE) {

  # Get SAFRAN parameters ---------------------------------------------------
  info <- fromJSON("https://api.geosas.fr/edr/collections/safran-isba/")
  params <- names(info$parameter_names) |> paste(collapse = ",")

  if (is.null(safran_path)) {
    safran_path <- system.file("extdata", "safran.gpkg", package = "hydros1mple")
  }
  if (is.null(maille_path)) {
    maille_path <- system.file("extdata", "SIM2.kml", package = "hydros1mple")
  }

  # Get mesh based on catchment area ----------------------------------------
  safran <- read_sf(safran_path) |>
    st_transform(st_crs(paste0("EPSG:", epsg)))

  ctm <- read_sf(ctm_path) %>%
    st_transform(st_crs(paste0("EPSG:", epsg))) %>%
    mutate(area = st_area(.))

  data <- st_intersection(ctm, safran) %>%
    mutate(area = st_area(.)) %>%
    mutate(proportion = area / sum(.$area))

  mesh <- setNames(data$proportion, data$cell)

  # Optionnal map -----------------------------------------------------------
  if (show_map) {
    p <- safran |>
      dplyr::slice(st_intersects(ctm, safran)[[1]]) %>%
      ggplot() +
      ggspatial::annotation_map_tile(zoom = 10, forcedownload = TRUE) +
      geom_sf(alpha = 0.4) +
      geom_sf(data = ctm, linewidth = 1, fill = "transparent") +
      geom_sf_label(aes(label = cell)) +
      ggspatial::annotation_scale(location = "br", bar_cols = c("grey30", "white")) +
      ggspatial::annotation_north_arrow(location = "tr") +
      theme_void()
    print(p)
  }

  # Download data -----------------------------------------------------------
  maille <- read_sf(maille_path)
  maille$Name <- as.integer(maille$Name)

  df <- list()
  for (id_maille in as.numeric(names(mesh))) {

    subset_geom <- maille[maille$Name == id_maille, ]
    coords <- st_coordinates(subset_geom)

    message("Téléchargement id safran : ", id_maille)
    message("Coordonnées x: ", coords[1], " y: ", coords[2])

    response <- GET(paste0(
      "https://api.geosas.fr/edr/collections/safran-isba/position?",
      "coords=POINT(", coords[1], "%20", coords[2], ")&",
      "crs=EPSG:4326&",
      "parameter-name=", params, "&f=CSV"
    ))

    if (response$status_code == 200) {
      message("Requête réussie.")
    } else {
      warning("Erreur lors de la requête pour la maille ", id_maille, ". Code : ", response$status_code)
      next
    }

    tmp <- content(response, as = "text", encoding = "UTF-8") |>
      read_csv(show_col_types = FALSE) |>
      mutate(time = as.Date(time)) |>
      setDT()

    cols <- setdiff(names(tmp), c("time", "x", "y"))
    tmp[, (cols) := lapply(.SD, \(x) x * mesh[as.character(id_maille)]), .SDcols = cols]

    df[[as.character(id_maille)]] <- tmp
  }

  # Aggregate ---------------------------------------------------------------
  result <- rbindlist(df)[, lapply(.SD, sum), by = time, .SDcols = cols]

  return(result)
}
