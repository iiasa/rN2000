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
n2000_unique_species <-
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

#' Get list of unique habitat names from Natura 2000 checklists
#'
#' @param x A [`character`] directory to the Natura 2000 csv files (i.e. containing 'Natura2000_end2019_HABITATS.csv'). Or a [`character`] file path to the Natura 2000 geopackage file ('Natura2000_end2019.gpkg').
#' @details Works with either geopackage (.gpkg) or csv (.csv) file extensions of downloaded data.
#' @details If you haven't downloaded the data yet, see `rN2000::n2000_getCSV()` or `rN2000::n2000_getGPKG()`
#' @return A \code{vector} of habitat names.
#' @author Matt Lewis
#' @export
n2000_unique_habitats <-
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
          c("Natura2000_end2019_HABITATS")
        )
      ret <-
        x_files %>%
        lapply(read.csv) %>%
        do.call(what = "rbind")
    }else{
      ret <-
        x %>%
          sf::st_read(layer = "HABITATS", quiet = T) %>%
          suppressWarnings()

    }

    ret <-
      ret %>%
      dplyr::mutate(DESCRIPTION = stringr::str_squish(DESCRIPTION)) %>%
      dplyr::filter(!duplicated(DESCRIPTION)) %>%
      dplyr::select(DESCRIPTION) %>%
      unlist() %>%
      as.character()

    return(ret)
  }

#' #' Checks and attempts to fix erroneous species names from Natura 2000 data
#' #' @description Uses the \code{taxize} package to look-up and (hopefully) fix incorrect species names.
#' #' @param x A \code{character} directory to the Natura 2000 csv files (i.e. containing 'Natura2000_end2019_HABITATS.csv'). Or a \code{character} file path to the Natura 2000 geopackage file ('Natura2000_end2019.gpkg').
#' #' @param overwrite A \code{logical} input - should the Natura 2000 data be overwritten with the fixed names? If no, only an internal R object is returned. Defaults to \code{TRUE}.
#' #' @param verbose A \code{logical} input. Be chatty? Defaults to \code{FALSE}.
#' #' @param ... Other parameters passed on to \code{taxize::tax_name()} or \code{taxize::synonyms()}
#' #' @author Matt Lewis
#' #' @export
#'
#'
#' n2000_format_species <- function(x, overwrite = T, verbose = F, ...){
#'
#'   assertthat::assert_that(is.logical(overwrite),
#'                           is.logical(verbose))
#'   assertthat::assert_that(requireNamespace("taxize", quietly = T),
#'                           msg = "The `taxize` package is required for this function. Please install it first.")
#'
#'   # get list of unique species
#'   uniq_sp <- n2000_unique_species(x)
#'
#'   # get rid of all unnecessary subsp or ssp
#'   uniq_sp_filt <-
#'     uniq_sp %>%
#'     sapply(
#'       function(x){
#'         x <-
#'           x %>%
#'           gsub("subsp.", "", .) %>%
#'           gsub("ssp.", "", .) %>%
#'           gsub("subsp", "", .) %>%
#'           gsub("ssp", "", .) %>%
#'           stringr::str_squish()
#'       }
#'     ) %>%
#'     unique()
#'
#'   # Ignore anything within brackets
#'   uniq_sp_filt <-
#'     uniq_sp_filt %>%
#'     sapply(
#'       function(x){
#'         x <-
#'           x %>%
#'           strsplit(split = "\\(") %>%
#'           unlist()
#'         y <-
#'           x[2:length(x)] %>%
#'           sapply(
#'             function(word){
#'               word <-
#'                 word %>%
#'                 stringr::str_squish() %>%
#'                 strsplit(split = "\\)") %>%
#'                 unlist()
#'
#'               word <- ifelse(length(word)==1L, "", word[length(word)])
#'             }
#'           )
#'
#'         z <-
#'           paste(
#'             x[1], paste(y, collapse = " ")
#'           ) %>%
#'           stringr::str_squish()
#'       }
#'     ) %>%
#'     unique()
#'
#'   # Split name into each word
#'   name_components <-
#'     uniq_sp_filt %>%
#'     sapply(
#'       strsplit,
#'       split = " "
#'     )
#'
#'   for(i in (1:length(name_components))){
#'     if(length(name_components[[i]]) > 2L){
#'       num_name_components <-
#'         name_components[[i]] %>%
#'         as.numeric() %>%
#'         suppressWarnings()
#'
#'       if(length(num_name_components[!is.na(num_name_components)]) > 0L){
#'         print(paste(name_components[[i]]))
#'       }
#'     }
#'   }
#'
#'   # processing dataframe
#'   name_df <-
#'     data.frame(
#'       query = uniq_sp_filt,
#'       rm_auth = NA,
#'       checked_binom = NA,
#'       subsp = NA
#'     )
#'
#'
#'   for(i in 1:nrow(name_df)){
#'     # first detect if there are any with authoritative names
#'
#'   }
#'
#'
#'   # Query all species using the taxize package
#'   o <-
#'     uniq_sp[1:5] %>%
#'       taxize::tax_name(
#'         get = 'species',
#'         messages = verbose,
#'         ...
#'       )
#'
#'   o$query[is.na(o$species)] %>%
#'     taxize::synonyms(
#'       db = "itis",
#'       messages = verbose
#'     )
#'
#'   # dir.create('data',showWarnings = FALSE)
#'   # save('df',file = 'data/NATURA2000_Species.rdata')
#'
#' }
