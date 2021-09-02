#' Map Natura 2000 sites using WMS services
#'
#' N.b. This function requires the \code{leaflet}, and \code{leaflet.extras2} packages to be installed and a functioning internet connection.
#'
#' @param title The [`character`] title of the WMS layer to be rendered. Options are: 'N2000', 'CDDA', and 'N2000_centroids'. Defaults to 'N2000'. See details.
#' @param clng The [`numeric`] centre-point longitude coordinate of the field of view. See details.
#' @param clat The [`numeric`] centre-point latitude coordinate of the field of view. See details.
#' @param czoom The [`numeric`] zoom level to use. See details.
#' @details `title` has three options: 'N2000' to render Natura 2000 sites, 'CDDA' to render sites listed on the Common Database on Designated Areas (i.e. nationally designated areas) or 'N2000_centroids' to render only the centroids of the Natura 2000 sites.
#' @details The default field of view for either \code{title = "N2000"} or \code{title = "CDDA"} focuses on the Schlosspark in Laxenburg, Austria. This is arbitrary, and does not by any means reflect any biases on the parts of the authors. The default for \code{title = "N2000_centroids"} gives view of most of Europe.
#' @examples
#'\dontrun{
#' n2000_mapwms()
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_mapwms <- function(title = 'N2000',
                         clng = NA,
                         clat = NA,
                         czoom = NA){
  assertthat::assert_that(requireNamespace(c("leaflet", "leaflet.extras2"), quietly = T))

  # Error checks
  assertthat::assert_that(is.character(title),
                          is.numeric(clng) | is.na(clng),
                          is.numeric(clat)| is.na(clat),
                          is.numeric(czoom) | is.na(czoom))

  if(title %in% c("N2000", "CDDA")){
    if(is.na(clng))clng <- 16.358805916581197
    if(is.na(clat))clat <- 48.06180471985718
    if(is.na(czoom))czoom <- 12
  }else{
    if(is.na(clng))clng <- sf::st_transform(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(europe_100km))), 4326)[[1]][1]
    if(is.na(clat))clat <- sf::st_transform(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(europe_100km))), 4326)[[1]][2]
    if(is.na(czoom))czoom <- 5
  }

  urls <- c(
    'N2000' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000_Dyna_WM/MapServer/WMSServer',
    'CDDA' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/CDDA_Dyna_WM/MapServer/WMSServer',
    'N2000_centroids' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000Centers_WM/MapServer/WMSServer'
  )
  if(title %notin% names(urls)){stop(paste0('WMS layer not found. Use one of ', paste(names(urls),collapse = '|') ))}
  what <- urls[title] %>% as.character()

  layers <-
    list(
      'N2000' = list(c(1:3),c(5:7)),
      'CDDA' = list(c(0:1,3), c(2), c(4)),
      'N2000_centroids' = list(c(0))
    )
  what_layer <-
    layers[title] %>%
    unlist(recursive = F)

  layer_name <-
    list(
      'N2000' = c("Birds Directive Sites (SPA)", "Habitat Directive Sites (pSCI, SCI, SAC)"),
      'CDDA' = c("Nationally Designated Areas (CDDA) polygons", "Nationally Designated Areas (CDDA) raster", "Nationally Designated Areas (CDDA) points"),
      'N2000_centroids' = c("Natura 2000 Centroids")
    )
  what_layer_name <-
    layer_name[title] %>%
    unlist() %>%
    as.character()

  mymap <-
    leaflet::leaflet() %>%
    leaflet::setView(lng = clng,lat = clat, zoom = czoom) %>%
    leaflet::addProviderTiles(provider = leaflet::providers$OpenStreetMap)

  for(i in 1:length(what_layer)){
    mymap <-
      mymap %>%
      leaflet.extras2::addWMS(
        baseUrl = what,
        layers = what_layer[i] %>% unlist() %>% as.character(),
        options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE, crs = 'EPSG:3857', info_format = "text/html", tiled = F),
        group = what_layer_name[i],
        popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300,)
      )
  }
  mymap <-
    mymap %>%
    leaflet::addLayersControl(
      baseGroups = "OSM",
      overlayGroups = what_layer_name,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  myLog('[Visualizing]', title)
  mymap

}

