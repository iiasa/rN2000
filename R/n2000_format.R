#' Load and reformat Natura 2000 definitions to a list.
#' @param x A [`character`] file path to the Natura 2000 data definitions file.
#' @author Matt Lewis
#' @author Martin Jung
#' @export
n2000_format_definitions <-
  function(x){
    assertthat::assert_that(
      is.character(x),
      file.exists(x),
      substr(x, (length(x)-4), length(x)) == ".xls",
      msg = "This file path doesn't seem to point to the correct file.
      If you haven't downloaded the definitions file, you can do so with `rN2000::n2000_getDefinitions()`")
    # Output list
    def <- list()
    for(sh in readxl::excel_sheets(x) ){
      def[[sh]] <- readxl::read_xls(x, sheet = sh)
    }

    return(def)
  }

#' Get list of unique species names from Natura 2000 checklists
#'
#' @param x A [`character`] directory to the Natura 2000 csv files (i.e. containing 'Natura2000_end2019_SPECIES.csv'). Or a [`character`] file path to the Natura 2000 geopackage file ('Natura2000_end2019.gpkg').
#' @details Works with either geopackage (.gpkg) or csv (.csv) file extensions of downloaded data.
#' @details If you haven't downloaded the data yet, see `rN2000::n2000_getCSV()` or `rN2000::n2000_getGPKG()`
#' @return A \code{vector} of species binomial names.
#' @author Matt Lewis
#' @export
n2000_unique_sp <-
  function(x){
    assertthat::assert_that(
      is.character(x),
      (file.exists(x) | dir.exists(x) | dir.exists(substr(x, 1, (nchar(x)-1)))),
      (substr(x, (nchar(x)-3), nchar(x)) == ".csv" | substr(x, (nchar(x)-4), nchar(x)) == ".gpkg"),
      msg = "This file path doesn't seem to point to the correct file(s).
      If you haven't downloaded the data yet, you can do so with `rN2000::n2000_getCSV()` or `rN2000::n2000_getGPKG()`")

    if(dir.exists(x) | dir.exists(substr(x, 1, (nchar(x)-1)))){
      x_files <-
        paste0(
          x,
          ifelse(substr(x, (nchar(x)-1), nchar(x)) == "/", "", "/"),
          c("Natura2000_end2019_SPECIES", "Natura2000_end2019_OTHERSPECIES")
        )
      ret <-
        x_files %>%
        lapply(read.csv) %>%
        do.call(what = "rbind")
    }else{
      ret <-
        dplyr::bind_rows(
          x %>%
            sf::st_read(layer = "SPECIES", quiet = T) %>%
            suppressWarnings(),
          x %>%
            sf::st_read(layer = "OTHERSPECIES", quiet = T) %>%
            suppressWarnings(),
        )

    }

    ret <-
      ret %>%
      dplyr::mutate(SPECIESNAME = stringr::str_squish(SPECIESNAME)) %>%
      dplyr::filter(!duplicated(SPECIESNAME)) %>%
      dplyr::select(SPECIESNAME) %>%
      unlist() %>%
      as.character()

    return(ret)
  }
