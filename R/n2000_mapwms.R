#' Maps outs the Natura 2000 sites using the commissions WMS services
#'
#' This function requires the {leaflet} package to be installed and a functioning internet connection
#'
#' @param title The title of the WMS layer to be rendered
#' @import leaflet
#' @examples
#'\dontrun{
#' n2000_mapwms(folder = 'myfolder', year = "2019")
#' }
#' @export
#' @author Martin Jung

n2000_mapwms <- function(title = 'N2000',
                         clng = 16.358805916581197,
                         clat = 48.06180471985718,
                         czoom = 12){
  urls <- c(
    'CDDA' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/CDDA_Dyna_WM/MapServer/WMSServer?request=GetCapabilities&service=WMS',
    'N2000' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000_Dyna_WM/MapServer/WMSServer?request=GetCapabilities&service=WMS',
    'N2000_centroids' = 'https://bio.discomap.eea.europa.eu/arcgis/services/ProtectedSites/Natura2000Centers_WM/MapServer/WMSServer?request=GetCapabilities&service=WMS'
  )
  # Error checks
  assertthat::assert_that(is.character(title),
                          is.numeric(clng),
                          is.numeric(clat),
                          is.numeric(czoom))
  if(title %notin% names(urls)){stop(paste0('WMS layer not found. Use one of ', paste(names(urls),collapse = '|') ))}
  myLog('[Visualizing]', title)

  what = urls[title]

  # FIXME: This still does not show up...
  leaflet() %>%
    setView(lng = clng,lat = clat, zoom = czoom) %>%
    addTiles(group = "OSM") %>%
    addWMSTiles(
      what,
      layers = "0",
      options = WMSTileOptions(format = "image/png", transparent = TRUE),
      group = title
    ) %>%
    addLayersControl(
      baseGroups = "OSM",
      overlayGroups = c("0"),
      options = layersControlOptions(collapsed = FALSE)
    )


}

