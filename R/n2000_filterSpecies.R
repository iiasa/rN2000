#' Create a species subset from the Natura 2000 data
#'
#' @param x The [`character] file path to the Natura 2000 geopackage
#' @param name The [`character`] latin name of a focal species
#' @return A [`sf`] object with the Natura 2000 sites containing this species
#' @examples
#'\dontrun{
#' n2000_species('/filepath/Natura2000_end2019.gpkg', name = 'Asio otus')
#' }
#' @export
#' @author Martin Jung

n2000_species <- function(x, name){
  # List and print the layers
  assertthat::assert_that(
    is.character(x),
    file.exists(x),
    assertthat::has_extension(x,'gpkg'),
    is.character(name))

  # Read species tables
  species_table <- bind_rows(
    suppressWarnings( sf::st_read(x, layer = "SPECIES") ) %>% dplyr::mutate(origin = 'SPECIES'),
    suppressWarnings( sf::st_read(x, layer = "OTHERSPECIES") ) %>% dplyr::mutate(origin = 'OTHER_SPECIES')
  )

  # Remove certain species without valid name
  species_table <- species_table %>% dplyr::filter(SPECIESNAME %notin% c('--NULL--')) %>%
    dplyr::filter(NONPRESENCEINSITE != '1')

  # Check that species exist in species table
  species_lists <- n2000_unique_sp(x)

  # Check that name is in species list
  assertthat::assert_that(name %in% species_lists,
                          anyNA(species_table$SPECIESNAME)==FALSE)

  # Table for species
  tab_sn <- species_table %>% dplyr::filter(SPECIESNAME == name) %>%
    dplyr::select(COUNTRY_CODE:SPECIESCODE,SPGROUP,POPULATION_TYPE,LOWERBOUND:INTRODUCTION_CANDIDATE) %>%
    dplyr::distinct()
  assertthat::assert_that(nrow(tab_sn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_sn), ' of ', name)
  return(tab_sn)
}

#' Create a habitat subset from the Natura 2000 data
#'
#' @param x The [`character] file path to the Natura 2000 geopackage
#' @param name The [`character`] name of the habitat
#' @return A [`sf`] object with the Natura 2000 sites containing this habitat
#' @examples
#'\dontrun{
#' n2000_habitat('/filepath/Natura2000_end2019.gpkg', name = 'Mountain hay meadows')
#' }
#' @export
#' @author Martin Jung

n2000_habitat <- function(x, name){
  # List and print the layers
  #st_layers(n2000_file)
  assertthat::assert_that(
    is.character(x),
    file.exists(x),
    assertthat::has_extension(x,'gpkg'),
    is.character(name))

  # Read species tables
  habitat_table <- sf::st_read(x, layer = "HABITATS") %>%
    suppressWarnings()

  # Remove certain habitats without valid name
  habitat_table <- habitat_table  %>% dplyr::filter(DESCRIPTION %notin% c('--NULL--'))
  # Remove sites where the habitat is no longer present
  habitat_table <- habitat_table %>% dplyr::filter(NON_PRESENCE_IN_SITE != '1')

  # Check that habitats exist in species table
  habitat_list <- stringr::str_squish ( as.character( unique(habitat_table$DESCRIPTION) ) )

  # Check that name is in species list
  assertthat::assert_that(name %in% habitat_list,
                          anyNA(habitat_table$HABITATCODE)==FALSE)

  # Table for species
  tab_hn <- habitat_table %>% dplyr::filter(DESCRIPTION == name)
  assertthat::assert_that(nrow(tab_sn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_hn), ' of ', name, ' sites')
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
