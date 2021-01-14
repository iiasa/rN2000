#' Create a species subset from the Natura2000 dataset
#'
#' @param n2000_file The file to the Natura2000 geopackage
#' @param sname The latin name of a focal species
#' @return A sf object with the Natura 2000 sites containing this species
#' @examples
#'\dontrun{
#' n2000_species('/media/martin/data/EuropeanBiodiversityData/N2000/Natura2000_end2019.gpkg', sname = 'Asio otus')
#' }
#' @export
#' @author Martin Jung

n2000_species <- function(n2000_file, sname){
  # List and print the layers
  #st_layers(n2000_file)
  assertthat::assert_that(file.exists(n2000_file),
                          !is.null(sname),
                          has_extension(n2000_file,'gpkg'))

  # Read species tables
  species_table <- bind_rows(
    suppressWarnings( sf::st_read(n2000_file, "SPECIES") ) %>% dplyr::mutate(origin = 'SPECIES'),
    suppressWarnings( sf::st_read(n2000_file, "OTHERSPECIES") ) %>% dplyr::mutate(origin = 'OTHER_SPECIES')
  )
  # Remove certain species without valid name
  species_table <- species_table %>% dplyr::filter(SPECIESNAME %notin% c('--NULL--')) %>%
    dplyr::filter(NONPRESENCEINSITE != '1')
  # Check that species exist in species table
  species_lists <- str_squish ( as.character( unique(species_table$SPECIESNAME) ) )
  # Check that name is in species list
  assertthat::assert_that(sname %in% species_lists,
                          anyNA(species_table$SPECIESNAME)==FALSE)

  # Table for species
  tab_sn <- species_table %>% dplyr::filter(SPECIESNAME == sname) %>%
    dplyr::select(COUNTRY_CODE:SPECIESCODE,SPGROUP,POPULATION_TYPE,LOWERBOUND:INTRODUCTION_CANDIDATE) %>%
    dplyr::distinct()
  assertthat::assert_that(nrow(tab_sn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_sn), ' of ', sname)
  return(tab_sn)
}

#' Create a habitat subset from the Natura2000 dataset
#'
#' @param n2000_file The file to the Natura2000 geopackage
#' @param hname The name of the habitat
#' @return A sf object with the Natura 2000 sites containing this habitat
#' @examples
#'\dontrun{
#' n2000_habitat('/media/martin/data/EuropeanBiodiversityData/N2000/Natura2000_end2019.gpkg', hname = 'Mountain hay meadows')
#' }
#' @export
#' @author Martin Jung

n2000_habitat <- function(n2000_file, hname){
  # List and print the layers
  #st_layers(n2000_file)
  assertthat::assert_that(file.exists(n2000_file),
                          !is.null(hname),
                          has_extension(n2000_file,'gpkg'))

  # Read species tables
  habitat_table <- sf::st_read(n2000_file, "HABITATS")

  # Remove certain species without valid name
  habitat_table <- habitat_table  %>% dplyr::filter(DESCRIPTION %notin% c('--NULL--'))
  # Remove sites where the habitat is no longer present
  habitat_table <- habitat_table %>% dplyr::filter(NON_PRESENCE_IN_SITE != '1')

  # Check that species exist in species table
  habitat_list <- str_squish ( as.character( unique(habitat_table$DESCRIPTION) ) )

  # Check that name is in species list
  assertthat::assert_that(hname %in% habitat_list,
                          anyNA(habitat_table$HABITATCODE)==FALSE)

  # Table for species
  tab_hn <- habitat_table %>% dplyr::filter(DESCRIPTION == hname)
  assertthat::assert_that(nrow(tab_sn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_hn), ' of ', hname, ' sites')
  return(tab_hn)
}

#' #' Function to check a species name against a list of supplied names
#' #' Crosscheck against taxize reference system.
#' #' The output will be saved internally as new object and thus this function does not need to be rerun
#' #'
#' #' @param species_lists
#' #' @keywords internal
#'
#'
#' matchTaxonomy <- function(species_lists){
#'   # Check that supplied vector is a species list
#'   assertthat::assert_that(is.vector(species_lists))
#'
#'   # Load the taxize package for matchup
#'   require(taxize)
#'
#'   df <- data.frame(
#'     raw_names = species_lists
#'   )
#'
#'   # Query all species using the taxize package
#'   o <- lapply(species_lists, function(x) { taxize::tax_name(x, get = 'species') } )
#'
#'   dir.create('data',showWarnings = FALSE)
#'   save('df',file = 'data/NATURA2000_Species.rdata')
#'
#' }
