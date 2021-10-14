#' Create a species subset from the Natura 2000 data
#'
#' @param x The \code{character} file path to the Natura 2000 geopackage
#' @param name The \code{character} latin name of a focal species
#' @param option The \code{character} type of data to return. Options are \code{"PO"} ('presence only' - sites where the species is present), \code{"PA"} ('presence absence' - sites where the species is present or absent), or \code{"abundance"}.
#' @param check.name \code{logical} Check whether the supplied name exists in the Natura 2000 data? Defaults to \code{FALSE}.
#' @return A \code{sf} object with the Natura 2000 sites containing this species
#' @examples
#'\dontrun{
#' n2000_filterSpecies('/filepath/Natura2000_end2019.gpkg', name = 'Asio otus', option = 'abundance')
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_filterSpecies <- function(x, name, option, check.name = F){
  # List and print the layers
  assertthat::assert_that(
    is.character(x),
    file.exists(x),
    assertthat::has_extension(x,'gpkg'),
    is.character(name),
    is.character(option),
    tolower(option) %in% c("PO", "PA", "abundance"),
    is.logical(check.name))
  # Match argument call
  option <- match.arg(option, c("PO", "PA", "abundance"), several.ok = FALSE)

  # Read species tables
  species_table <- dplyr::bind_rows(
    suppressWarnings( sf::st_read(x, layer = "SPECIES", quiet = T) ) %>% dplyr::mutate(origin = 'SPECIES'),
    suppressWarnings( sf::st_read(x, layer = "OTHERSPECIES", quiet = T) ) %>% dplyr::mutate(origin = 'OTHER_SPECIES')
  )

  # Remove certain species without valid name
  species_table <-
    species_table %>%
    dplyr::filter(SPECIESNAME %notin% c('--NULL--')) %>%
    dplyr::filter(NONPRESENCEINSITE != '1')

  # Check that species exist in species table
  if(check.name == T){
    species_lists <-
      species_table %>%
      dplyr::mutate(SPECIESNAME = stringr::str_squish(SPECIESNAME)) %>%
      dplyr::filter(!duplicated(SPECIESNAME)) %>%
      dplyr::select(SPECIESNAME) %>%
      unlist() %>%
      as.character()

    # Check that name is in species list
    assertthat::assert_that(name %in% species_lists,
                            anyNA(species_table$SPECIESNAME)==FALSE)
  }

  # Table for species
  tab_sn <-
    species_table %>%
    dplyr::filter(SPECIESNAME == name) %>%
    dplyr::select(COUNTRY_CODE:SPECIESCODE,SPGROUP,POPULATION_TYPE,LOWERBOUND:INTRODUCTION_CANDIDATE) %>%
    dplyr::distinct()
  assertthat::assert_that(nrow(tab_sn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_sn), ' records of ', name)

  if(option == "PO"){
    ret <-
      x %>%
      sf::st_read(
        query = paste0('SELECT * FROM NaturaSite_polygon WHERE SITECODE IN (',
                       paste(paste0('"',unique(tab_sn$SITECODE),'"'),
                             collapse = ', '), ");"),
        quiet = T)
  } else if(option == "PA"){
    ret <-
      x %>%
      sf::st_read(
        layer = "NaturaSite_polygon",
        quiet = T) %>%
      dplyr::mutate(PA = ifelse(SITECODE %in% tab_sn$SITECODE,
                                "Present",
                                "Absent"))
  } else {
    ret <-
      x %>%
      sf::st_read(
        layer = "NaturaSite_polygon",
        quiet = T) %>%
      dplyr::inner_join(tab_sn, by = "SITECODE")
    ret$LOWERBOUND[is.na(ret$LOWERBOUND)] <- 0
    ret$UPPERBOUND[is.na(ret$UPPERBOUND)] <- 0
    ret <-
      ret %>%
      dplyr::mutate(
        LOWERBOUND = as.numeric(LOWERBOUND),
        UPPERBOUND = as.numeric(UPPERBOUND)) %>%
      dplyr::mutate(
        AVGPOP = rowMeans(dplyr::select(as.data.frame(.), LOWERBOUND, UPPERBOUND)))
  }

  return(ret)
}

#' Create a habitat subset from the Natura 2000 data
#'
#' @param x The \code{character} file path to the Natura 2000 geopackage
#' @param name The \code{character} name of the habitat
#' @return A \code{sf} object with the Natura 2000 sites containing this habitat
#' @examples
#'\dontrun{
#' n2000_filterHabitats('/filepath/Natura2000_end2019.gpkg', name = 'Mountain hay meadows')
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_filterHabitats <- function(x, name){
  # List and print the layers
  #st_layers(n2000_file)
  assertthat::assert_that(
    is.character(x),
    file.exists(x),
    assertthat::has_extension(x,'gpkg'),
    is.character(name))

  # Read species tables
  habitat_table <-
    sf::st_read(x, layer = "HABITATS", quiet = T) %>%
    suppressWarnings()

  # Remove certain habitats without valid name
  habitat_table <-
    habitat_table  %>%
    dplyr::filter(DESCRIPTION %notin% c('--NULL--')) %>%
    # Remove sites where the habitat is no longer present
    dplyr::filter(NON_PRESENCE_IN_SITE != '1')

  # Check that habitats exist in species table
  habitat_list <- stringr::str_squish ( as.character( unique(habitat_table$DESCRIPTION) ) )

  # Check that name is in species list
  assertthat::assert_that(name %in% habitat_list,
                          anyNA(habitat_table$HABITATCODE)==FALSE)

  # Table for species
  tab_hn <- habitat_table %>% dplyr::filter(DESCRIPTION == name)
  assertthat::assert_that(nrow(tab_hn)>0)

  myLog('[Filtering] ','Prepared ', nrow(tab_hn), ' records of ', name, ' sites')

  ret <-
    x %>%
    sf::st_read(
      query = paste0('SELECT * FROM NaturaSite_polygon WHERE SITECODE IN (', paste(paste0('"',unique(tab_hn$SITECODE),'"'), collapse = ', '), ");"),
      quiet = T)
  return(ret)
}
