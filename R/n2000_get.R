#' Set or download the N2000 database in csv format to a given folder
#'
#' @param folder A [`character`] directory path to download the Natura 2000 data set to. Automatic checks to see if the data already exist in the folder (can be overridden by specifying `force = T`).
#' @param year (optional). The [`character`] year version of the Natura 2000 data set to check if it already exists. Defaults to the most recently available version (2019).
#' @param force (optional). [`logical`] should a new download be forced even if data already exists in the directory? Defaults to `FALSE`.
#' @param nlink (normally not altered). The [`character`] direct download link for Natura 2000.
#' @examples
#'\dontrun{
#' n2000_getCSV(folder = 'myfolder', year = "2019")
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_getCSV <-
  function(
    folder,
    year = "2019",
    force = FALSE,
    nlink = 'https://www.eea.europa.eu/data-and-maps/data/natura-11/natura-2000-tabular-data-12-tables/natura-2000-comma-separated-values-files/at_download/file'
    ){

  folder2 <-
    ifelse(
      substr(folder, nchar(folder), nchar(folder)) == "/",
      substr(folder, 1, (nchar(folder) -1)),
      folder
    )

  # Check
  assertthat::assert_that(
    assertthat::is.dir(folder2),
    is.character(year),
    is.character(nlink),
    substr(nlink, 1, 8) == "https://")

  # Key files and sources
  file_zipped <- paste0("Natura2000_end",year,"_csv.zip")

  # Load files in folder
  lf <- list.files(folder, full.names = T, recursive = T)

  # if zip file already there, unzip and delete
  if(file_zipped %in% basename(lf) & force == F){
    unzip(zipfile = lf[ grep(file_zipped, basename(lf)) ],exdir = paste0(folder, gsub(".zip", "", file_zipped)),overwrite = TRUE)
    file.remove(lf[ grep(file_zipped, basename(lf)) ])
  } else {
    # Check if some of the zipped files do not exist
    if( paste0('Natura2000_end',year,'_SPECIES.csv') %notin% basename(lf) || force == T) {
      myLog('Downloading new Natura 2000 data')
      o<- try({ download.file(url = nlink,
                    destfile = paste0(folder,'/', file_zipped), mode = "wb")
      })
      if(class(o)=='try-error' || o != 0){ stop('Natura 2000 file could not be downloaded. Change download link (`nlink`)')}
      # Unzip and delete the downloaded file
      lf <- list.files(folder,full.names = T)
      unzip(zipfile = lf[ grep(file_zipped, basename(lf)) ],exdir = paste0(folder, gsub(".zip", "", file_zipped)),overwrite = TRUE)
      file.remove(lf[ grep(file_zipped, basename(lf)) ])
    }else{
      cat("Up to date Natura 2000 data already appears to be in the selected folder. No download needed.
          If you want to redownload the data, set `force = T`")
    }
  }
}

#' Set or download the N2000 database in geopackage format to a given folder
#'
#' @param folder A [`character`] directory path to download the Natura 2000 data set to. Automatic checks to see if the data already exist in the folder (can be overridden by specifying `force = T`).
#' @param year (optional). The [`character`] year version of the Natura 2000 data set to check if it already exists. Defaults to the most recently available version (2019).
#' @param force (optional). [`logical`] should a new download be forced even if data already exists in the directory? Defaults to `FALSE`.
#' @param nlink (normally not altered). The [`character`] direct download link for Natura 2000.
#' @examples
#'\dontrun{
#' n2000_getGPKG(folder = 'myfolder', year = "2019")
#' }
#' @export
#' @author Martin Jung
#' @author Matt Lewis

n2000_getGPGK <- function(folder,
                         year = "2019",
                         force = FALSE,
                         nlink = 'https://cmshare.eea.europa.eu/s/GkqdcbbsYmmBSEQ/download'){
  folder2 <-
    ifelse(
      substr(folder, nchar(folder), nchar(folder)) == "/",
      substr(folder, 1, (nchar(folder) -1)),
      folder
    )

  # Check
  assertthat::assert_that(
    assertthat::is.dir(folder2),
    is.character(year),
    is.character(nlink),
    substr(nlink, 1, 8) == "https://"
    )

  # Key files and sources
  file_g <- paste0("Natura2000_end",year,"_gpkg.zip")

  # Load files in folder
  lf <- list.files(folder, full.names = T, recursive = T)

  # if zip file already there, unzip and delete
  if(file_g %in% basename(lf) & force == FALSE){
    unzip(zipfile = lf[ grep(file_g, basename(lf)) ], exdir = paste0(folder, gsub(".zip", "", file_g)), overwrite = TRUE)
    file.remove(lf[ grep(file_g, basename(lf)) ])
  } else {
    # Check if some of the zipped files do not exist
    if( paste0('Natura2000_end',year,'.gpkg') %notin% basename(lf) | force == T) {
      myLog('Downloading new Natura 2000 data')
      o<- try({ download.file(url = nlink,
                              destfile = paste0(folder,'/', file_g), mode = "wb")
      })
      if(class(o)=='try-error' || o != 0){ stop('Natura 2000 file could not be downloaded. Change download link')}
      # Unzip and delete the downloaded file
      lf <- list.files(folder,full.names = T)
      unzip(zipfile = lf[ grep(file_g, basename(lf)) ], exdir = paste0(folder, gsub(".zip", "", file_g)), overwrite = TRUE)
      file.remove(lf[ grep(file_g, basename(lf)) ])
    }else{
      cat("Up to date Natura 2000 data already appears to be in the selected folder. No download needed.
          If you want to redownload the data, set `force = T`")
    }
  }
}

#' Download Natura 2000 definitions
#' @param folder A [`character`] directory path to download the Natura 2000 data definitions to. Automatically checks to see if the data already exist in the folder (can be overridden by specifying `force = T`).
#' @param year (optional). The [`character`] year version of the Natura 2000 data set to check if it already exists. Defaults to the most recently available version (2019).
#' @param force (optional). [`logical`] should a new download be forced even if data already exists in the directory? Defaults to `FALSE`.
#' @param nlink (normally not altered). The [`character`] direct download link for Natura 2000 definitions file.
#' @author Matt Lewis
#' @author Martin Jung
#' @export

n2000_getDefinitions <- function(
  folder,
  year = "2019",
  force = FALSE,
  nlink = 'https://www.eea.europa.eu/data-and-maps/data/natura-11/table-definitions/table-definitions-xls-file/at_download/file'){

  folder2 <-
    ifelse(
      substr(folder, nchar(folder), nchar(folder)) == "/",
      substr(folder, 1, (nchar(folder) -1)),
      folder
    )

  # Check
  assertthat::assert_that(
    assertthat::is.dir(folder2),
    is.character(year),
    is.character(nlink),
    substr(nlink, 1, 8) == "https://"
  )

  # Key files and sources
  file_g <- paste0("Natura2000_end",year,"_dataset_definitions.xls")

  # Load files in folder
  lf <- list.files(folder, full.names = T, recursive = T)

  # if zip file already there, unzip and delete
  if(file_g %notin% basename(lf) | force == T) {
    myLog('Downloading new Natura 2000 data definitions')
    o <- try({
      download.file(
        url = nlink,
        destfile = paste0(folder, '/', file_g),
        mode = "wb"
      )
    })
    if (class(o) == 'try-error' ||
        o != 0) {
      stop('Natura 2000 file could not be downloaded. Change download link')
    }
  } else{
    cat(
      "Up to date Natura 2000 data definitions already appears to be in the selected folder. No download needed.
          If you want to redownload the data, set `force = T`"
    )
  }
}


#' Show N2000 database definitions
#'
#' @param table (optional). Table of the definitions (optional)
#' @examples
#'\dontrun{
#' def_list <- n2000_getdefinitions()
#' def_list$HABITATS
#' }
#' @export
#' @author Martin Jung

n2000_showdefinitions <- function(table = NULL){
  load('data/NATURA2000_definitions.rdata')
  if(!is.null(table)){
    assert_that(is.character(table),
                table %in% names(def))
    # Reassign
    assign('def',def[[table]] )
  }
}
