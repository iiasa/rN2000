#' Map Natura 2000 sites using WMS services
#'
#' N.b. This function requires the {`leaflet`} package to be installed and a functioning internet connection.
#'
#' @param title The [`character`] title of the WMS layer to be rendered. Options are: 'N2000', 'CDDA', and 'N2000_centroids'. Defaults to 'N2000'. See details.
#' @param clng The [`numeric`] centre-point longitude coordinate of the field of view. See details.
#' @param clat The [`numeric`] centre-point latitude coordinate of the field of view. See details.
#' @param czoom The [`numeric`] zoom level to use. Defaults to 12 (see details).
#' @details `title` has three options: 'N2000' to render Natura 2000 sites, 'CDDA' to render sites listed on the Common Database on Designated Areas (i.e. nationally designated areas) or 'N2000_centroids' to render only the centroids of the Natura 2000 sites.
#' @details The default field of view for the returned image focuses on the Schlosspark in Laxenburg, Austria. This is arbitrary, and does not by any means reflect any biases on the parts of the authors.
#' @examples
#'\dontrun{
#' n2000_mapwms()
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_mapwms <- function(title = 'N2000',
                         clng = 16.358805916581197,
                         clat = 48.06180471985718,
                         czoom = 12){
  urls <- c(
    'CDDA' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/CDDA_Dyna_WM/MapServer/WMSServer',
    'N2000' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000_Dyna_WM/MapServer/WMSServer',
    'N2000_centroids' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000Centers_WM/MapServer/WMSServer'
  )
  # Error checks
  assertthat::assert_that(is.character(title),
                          is.numeric(clng),
                          is.numeric(clat),
                          is.numeric(czoom))
  if(title %notin% names(urls)){stop(paste0('WMS layer not found. Use one of ', paste(names(urls),collapse = '|') ))}
  myLog('[Visualizing]', title)

  what <- urls[title] %>% as.character()

  leaflet::leaflet() %>%
    leaflet::setView(lng = clng,lat = clat, zoom = czoom) %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addWMSTiles(
      baseUrl = what,
      layers = "0",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE, crs = 'EPSG:3857'),
      group = title
    ) %>%
    leaflet::addLayersControl(
      baseGroups = "OSM",
      overlayGroups = c("0"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )


}

