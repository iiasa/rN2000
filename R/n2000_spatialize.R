#' Prepares a target species multipolygon feature for modelling
#' using
#' @param species A data.frame obtained via the n2000_filterSpecies function
#' @param metric How is biodiversity encoded in the output. As presence only ('po'), presence absence ('pa') or a population estimate ('abundance')
#' @param abundance_bound Which estimate to use for metric option 'abundance'. Default is 'UPPERBOUND'
#' @param what What should be created. Possible options ('polygon','raster')
#' @param ref_grid Which European reference grid to use. Default is '1km'
#' @return A spatial object of the respective species. The
#' @examples
#'\dontrun{
#' species <- n2000_species('/media/martin/data/EuropeanBiodiversityData/N2000/Natura2000_end2019.gpkg',
#'                     sname = 'Asio otus')
#' o <- n2000_SpatialSpecies(species, 'Asio_otus', metric = 'pa', what = 'raster' )
#' plot(o)
#' }
#' @export
#' @author Martin Jung

n2000_SpatialSpecies <- function(species,
                                 metric = 'pa', abundance_bound = 'UPPERBOUND',
                                 what = 'polygon',ref_grid = '1km'){
  # Checks
  assertthat::assert_that(
    is.data.frame(species),
    is.character(ofname),
    is.character(what),is.character(metric),
    metric %in% c('pa','po','abundance'),
    what %in% c('polygon','raster')
  )

  myLog('[Preparing] ','-> Data for ', as.character(unique(species$SPECIESNAME)), ' for metric: ',metric )
  # Join with N2k polygon data
  sites <- sf::st_read(n2000_file,'NaturaSite_polygon')
  # Now preparing
  if(metric == 'po'){
    # Join in
    out <- sites %>% inner_join(., species, by = 'SITECODE') %>%
      # Codify the presence in the output
      dplyr::mutate(observed = 1)

  } else if(metric == 'pa'){
    # Join in
    out <- bind_rows(
      sites %>% inner_join(., species, by = 'SITECODE') %>%
      # Codify the presence in the output
      dplyr::mutate(observed = 1),
      ## The absence data
      sites %>% anti_join(., species, by = 'SITECODE') %>%
        # Codify the presence in the output
        dplyr::mutate(observed = 0)
    )
    # Fill the absence records with the species name
    out$SPECIESNAME[is.na(out$SPECIESNAME)] <- as.character(unique(species$SPECIESNAME))

  } else if(metric == 'abundance'){
    # Join in for data with given abundance estimates
    out <- sites %>% inner_join(., species, by = 'SITECODE') %>%
      tidyr::drop_na({{abundance_bound}})
    #FIXME: Note that bounds can be 0 values too. Thus resulting in zero-inflated Poisson
    #FIXME: Also highlight difference in data quality, counting units and population. Could support additional filters
      # If the bound value is unknown, give it 1 at least. Otherwise insert bound
    out$observed <- as.numeric(as.character(out[[{{abundance_bound}}]]))
    out$observed <- ifelse(out$observed == 0, 1, out$observed)
  }

  assertthat::assert_that(nrow(out)>0,
                          has_name(out,'observed'))

  # Load the reference grid
  #TODO: Check that reference grids exist
  load(paste0('data/RefGrid_EU_',ref_grid,'.rdata'))
  ref <- get(paste0('ref',ref_grid))
  load(paste0('data/RefRaster_EU_',ref_grid,'.rdata'))
  ref.grid <- get(paste0('ras',ref_grid))

  # Format the output
  out <- out %>%
    # Transform to reference projection if necessary
    st_transform(crs = st_crs(ref)) %>%
    dplyr::select(SITECODE,SITENAME,MS,SPECIESNAME,SPECIESCODE,observed, geom)

  if(what == 'polygon'){
    # Intersect with reference grid to get the codes
    out <- st_intersection(ref,out)
  } else {
    # Rasterize the reference grid
    if( "fasterize" %in% installed.packages()[,"Package"] ){
      # When Fasterize is available use that
      out <- fasterize::fasterize(out, ref.grid,field = 'observed')
    } else {
      # otherwise the slow raster package
      out <- rasterize(out, ref.grid,field = 'observed')
    }
    assertthat::assert_that(
      class(out)=='RasterLayer',
      cellStats(out,'max') >= 1
                )
  }
  return(out)
}

#' Downloads the EU reference grid and saves it as data.object
#' The European-wide reference grid is only at 10km resolution
#' @param url The url to the download site
#' @return Saves as data object the reference grid at varying resolutions
#' @author Martin Jung
#' @examples
#' \dontrun{
#' prep_EUgrid('https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/europe-10-km-100-km/at_download/file')
#' }
#' @keywords internal

prep_EUgrid <- function(url){
  # Check
  assertthat::assert_that(is.character(url))

  # Create a temp file
  tmp <- tempfile()
  tmp.dir <- tempdir()
  download.file(url, destfile = tmp)
  # TODO: Do unzipping and downloading for 1km
  stop('Not yet coded, run manually')

  ref10km <- st_read('/media/martin/data/EuropeanBiodiversityData/EuropeReferenceGrid/europe_10km.shp')
  ref100km <- st_read('/media/martin/data/EuropeanBiodiversityData/EuropeReferenceGrid/europe_100km.shp')

  dir.create('data',showWarnings = FALSE)
  save(ref10km,file = 'data/RefGrid_EU_10km.rdata')
  save(ref100km,file = 'data/RefGrid_EU_100km.rdata')

  # Biogeoregions
  biogeo <- st_read('/media/martin/data/EuropeanBiodiversityData/BiogeoRegions2016_shapefile/BiogeoRegions2016.shp')
  biogeo$burn <- 1
  # Rasterization objects
  tr <- raster(ext = extent(biogeo),
               crs = proj4string(as(biogeo,'Spatial')),
               resolution = 1000
               )
  ras1km <- fasterize::fasterize(biogeo, tr, field = 'burn')
  ras10km <- raster::aggregate(ras1km, fact = 10)
  ras10km <- raster::mask(ras10km, biogeo) # Mask with biogeographical regions
  ras50km <- raster::aggregate(ras10km, fact = 5)
  ras50km <- raster::mask(ras50km, biogeo) # Mask with biogeographical regions
  save(ras1km, file = 'data/RefRaster_EU_1km.rdata')
  save(ras10km, file = 'data/RefRaster_EU_10km.rdata')
  save(ras50km, file = 'data/RefRaster_EU_50km.rdata')
}
