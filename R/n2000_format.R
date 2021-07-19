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

#' Checks and attempts to fix erroneous species names from Natura 2000 data
#' @description Uses the \code{taxize} package to look-up and (hopefully) fix incorrect species names.
#' @param x A \code{character} directory to the Natura 2000 csv files (i.e. containing 'Natura2000_end2019_HABITATS.csv'). Or a \code{character} file path to the Natura 2000 geopackage file ('Natura2000_end2019.gpkg').
#' @param overwrite A \code{logical} input - should the Natura 2000 data be overwritten with the fixed names? If no, only an internal R object is returned. Defaults to \code{TRUE}.
#' @param verbose A \code{logical} input. Be chatty? Defaults to \code{FALSE}.
#' @param ... Other parameters passed on to \code{taxize::tax_name()} or \code{taxize::synonyms()}
#' @author Matt Lewis
#' @export


n2000_format_species <- function(x, overwrite = T, verbose = F, ...){

  assertthat::assert_that(is.logical(overwrite),
                          is.logical(verbose))
  assertthat::assert_that(requireNamespace("taxize", quietly = T),
                          msg = "The `taxize` package is required for this function. Please install it first.")

  # get list of unique species
  uniq_sp <- n2000_unique_species(x)

  ## TODO: Get list of all taxonomic options (e.g. subsp., var., ex.) and rm make a df from this with next name
  tax_options <-
    list(
      species = c("species", "sp", "sp.", "spp", "spp."),
      infrasp = c("infraspecies"),
      subsp = c("subsp.", "ssp.", "subsp", "ssp", "subspp.", "subspp", "sspp.", "sspp"),
      var = c("var.", "var", "variety", "varietas"),
      subvar = c("subvar.", "subvar", "subvariety", "subvarietas"),
      form = c("form", "f.", "forma", "morph"),
      subf = c("subforma", "subf.", "subform"),
      cross = c("x", "×")
    )

  tax_df <-
    matrix(
      ncol = length(tax_options)+5,
      nrow = length(uniq_sp)
    ) %>%
    as.data.frame()

  colnames(tax_df) <- c("input", "split_name", "genus", names(tax_options), "taxize_input", "taxize_output")

  tax_df$input <- uniq_sp

  # Ignore anything within brackets (either '(', ')' or '[',']')
  uniq_sp_filt <-
    uniq_sp %>%
    sapply(
      function(x){
        x <-
          x %>%
          strsplit(split = "\\(") %>%
          unlist()
        y <-
          x[2:length(x)] %>%
          sapply(
            function(word){
              word <-
                word %>%
                stringr::str_squish() %>%
                strsplit(split = "\\)") %>%
                unlist()

              word <- ifelse(length(word)==1L, "", word[length(word)])
            }
          )

        z <-
          paste(
            x[1], paste(y, collapse = " ")
          ) %>%
          stringr::str_squish()

        xx <-
          z %>%
          strsplit(split = "\\[") %>%
          unlist()
        yy <-
          xx[2:length(xx)] %>%
          sapply(
            function(word){
              word <-
                word %>%
                stringr::str_squish() %>%
                strsplit(split = "\\]") %>%
                unlist()

              word <- ifelse(length(word)==1L, "", word[length(word)])
            }
          )

        zz <-
          paste(
            xx[1], paste(yy, collapse = " ")
          ) %>%
          stringr::str_squish()
      }
    )

  # Get rid of asterisks
  uniq_sp_filt <-
    uniq_sp_filt %>%
    gsub("\\*", "", .)

  # Split name into each word
  tax_df$split_name <-
    uniq_sp_filt %>%
    lapply(
      strsplit,
      split = " "
    ) %>%
    lapply(
      function(x){
        x <-
          x[x %notin% c(",", ".", "-", ";")]
      })

  for(i in 1:nrow(tax_df)){
    # Get split name
    split_name <- unlist(tax_df$split_name[i])
    # Add genus and species info
    tax_df$genus[i] <- split_name[1]
    tax_df$species[i] <-
      ifelse(
        split_name[2] %in% tax_options$species,
        NA,
        split_name[2]
      )
    # If length > 2
    if(length(split_name) > 2L){
      # Check if we have any flags for adding taxonomic info (e.g. subsp.)
      if(any(unlist(tax_options) %in% split_name)){
        for(j in 2:length(tax_options)){
          if(any(unlist(tax_options[j]) %in% split_name)){
            tax_df[i, names(tax_options[j])] <- split_name[which(split_name %in% unlist(tax_options[j]))+1]
          }
        }
      }else{
        # Does it contain a ',', indicating this is an authority
        if(grepl(",", split_name[3]) == T){
          split_name <- split_name[-c(3,4)]
          if(length(split_name) == 2L){next()}
        }else if(length(split_name) == 3L &
                 tolower(split_name[3]) == split_name[3]){
          next()
        }else if((split_name[3] == "all" & split_name[4] == "others") |
                 split_name[3] == "Complex"){
          split_name <- split_name[-c(3,4)]
          if(length(split_name) == 2L){next()}
        }else if(split_name[3] %in% c("Rebel")){
          split_name <- split_name[-c(3,4)]
          if(length(split_name) == 2L){next()}
        }
        stop()
      }
    }
  }

  # Get rid of components which are either solely punctuation, or which contain ',', '.' as these are likely authority names
  # NB not '/' as this indicates ambiguity for name
  name_components <-
    name_components %>%
    lapply(
      function(x){
        x <-
          x[x %notin% c(",", ".", "-", ";")]

        x <-
          x[grepl("[.,]", x) == F]
      }
    )

  ## to remove later:
  b <-
    name_components %>%
    sapply(length)

  name_components[b >2] %>%
    sapply(function(x)x<-x[3]) %>%
    unique() %>%
    sort()

  for(i in (1:length(name_components))){
    if(length(name_components[[i]]) > 2L){
      num_name_components <-
        name_components[[i]] %>%
        as.numeric() %>%
        suppressWarnings()

      if(length(num_name_components[!is.na(num_name_components)]) > 0L){
        print(paste(name_components[[i]]))
      }
    }
  }


  # processing dataframe
  name_df <-
    data.frame(
      query = uniq_sp_filt,
      rm_auth = NA,
      checked_binom = NA,
      subsp = NA
    )


  for(i in 1:nrow(name_df)){
    # first detect if there are any with authoritative names

  }


  # Query all species using the taxize package
  o <-
    uniq_sp[1:5] %>%
      taxize::tax_name(
        get = 'species',
        messages = verbose,
        ...
      )

  o$query[is.na(o$species)] %>%
    taxize::synonyms(
      db = "itis",
      messages = verbose
    )

  # dir.create('data',showWarnings = FALSE)
  # save('df',file = 'data/NATURA2000_Species.rdata')

}
