#' 10-km grid of Europe
#'
#' A grid of Europe at 10-km resolution in EPSG 3035. Based on FAO GAUL level 0 boundary polygons.
#'
#' @format A \code{sf} polygon.
#' \describe{
#'   \item{CellCode}{code for individual cells}
#'   \item{EofOrigin}{distance East of the data origin(centroid) in m}
#'   \item{NfOrigin}{distance North of the data origin(centroid) in m}
#' }
#' @author European Environment Agency (EEA)
#' @source \url{https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2}
"europe_10km"

#' 100-km grid of Europe
#'
#' A grid of Europe at 100-km resolution in EPSG 3035. Based on FAO GAUL level 0 boundary polygons.
#'
#' @format A \code{sf} polygon.
#' \describe{
#'   \item{CellCode}{code for individual cells}
#'   \item{EofOrigin}{distance East of the data origin(centroid) in m}
#'   \item{NfOrigin}{distance North of the data origin(centroid) in m}
#' }
#' @author European Environment Agency (EEA)
#' @source \url{https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2}
"europe_100km"

#' European country administrative boundaries
#'
#' @description Natural Earth data at \code{scale = 110} for countries within a European Bounding Box. Obtained through the \code{rnaturalearth} package at \code{scale = 110}. Reprojected to \code{ETRS89-extended / LAEA Europe} and clipped to a bounding box around Europe using the internal \code{N2000:::get_EuropeanCountries()} function.
#'
#' @format \code{sf} polygons.
#' @author Natural Earth
#' @source \url{https://www.naturalearthdata.com/downloads/}
"europe_countries_lowres"

#' European country administrative boundaries
#'
#' @description Natural Earth data at \code{scale = 50} for countries within a European Bounding Box. Obtained through the \code{rnaturalearth} package at \code{scale = 50}. Reprojected to \code{ETRS89-extended / LAEA Europe} and clipped to a bounding box around Europe using the internal \code{N2000:::get_EuropeanCountries()} function.
#'
#' @format \code{sf} polygons.
#' @author Natural Earth
#' @source \url{https://www.naturalearthdata.com/downloads/}
"europe_countries_midres"
