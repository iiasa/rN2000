#' Set or download the N2000 database in csv format to a given folder
#'
#' @param folder A folder with the Natura 2000 dataset
#' @param year The latest version of the Natura 2000 dataset
#' @param nlink This should normally not be altered. Direct download link for Natura 2000
#' @examples
#'\dontrun{
#' n2000_getCSV(folder = 'myfolder', year = "2019")
#' }
#' @export
#' @author Martin Jung

n2000_getCSV <- function(folder,
                      year = "2019",
                      n_link = 'https://www.eea.europa.eu/data-and-maps/data/natura-11/natura-2000-tabular-data-12-tables/natura-2000-comma-separated-values-files/at_download/file'){
  folder2 <-
    ifelse(
      substr(folder, nchar(folder), nchar(folder)) == "/",
      substr(folder, 1, (nchar(folder) -1)),
      folder
    )

  # Check
  assert_that(is.dir(folder2),
              is.character(year))

  # Key files and sources
  file_zipped <- paste0("Natura2000_end",year,"_csv.zip")

  # Load files in folder
  lf <- list.files(folder,full.names = T)

  # if zip file already there, unzip and delete
  if(file_zipped %in% basename(lf)){
    unzip(zipfile = lf[ grep(file_zipped, basename(lf)) ],exdir = folder,overwrite = TRUE)
    file.remove(lf[ grep(file_zipped, basename(lf)) ])
  } else {
    # Check if some of the zipped files do not exist
    if( paste0('Natura2000_end',year,'_SPECIES.csv') %notin% basename(lf) ) {
      myLog('No Natura 2000 data found. Redownloading')
      try({ o <- download.file(url = n_link,
                    destfile = paste0(folder,'/', file_zipped))
      })
      if(class(o)=='try-error' || o != 0){ stop('Natura 2000 file could not be downloaded. Change download link')}
      # Unzip and delete the downloaded file
      unzip(zipfile = lf[ grep(file_zipped, basename(lf)) ],exdir = folder,overwrite = TRUE)
      file.remove(lf[ grep(file_zipped, basename(lf)) ])
    }
  }
}

#' Set or download the N2000 database in geopackage format to a given folder
#'
#' @param folder A folder with the Natura 2000 dataset
#' @param year The latest version of the Natura 2000 dataset
#' @param nlink This should normally not be altered. Direct download link for Natura 2000
#' @examples
#'\dontrun{
#' n2000_getGPKG(folder = 'myfolder', year = "2019")
#' }
#' @export
#' @author Martin Jung

n2000_getGPGK <- function(folder,
                         year = "2019",
                         n_link = 'https://cmshare.eea.europa.eu/s/GkqdcbbsYmmBSEQ/download'){
  folder2 <-
    ifelse(
      substr(folder, nchar(folder), nchar(folder)) == "/",
      substr(folder, 1, (nchar(folder) -1)),
      folder
    )

  # Check
  assert_that(is.dir(folder2),
              is.character(year))

  # Key files and sources
  file_g <- paste0("Natura2000_end",year,"_gpkg.zip")

  # Load files in folder
  lf <- list.files(folder,full.names = T)

  # if zip file already there, unzip and delete
  if(file_g %in% basename(lf)){
    unzip(zipfile = lf[ grep(file_g, basename(lf)) ], exdir = folder, overwrite = TRUE)
    file.remove(lf[ grep(file_g, basename(lf)) ])
  } else {
    # Check if some of the zipped files do not exist
    if( paste0('Natura2000_end',year,'.gpkg') %notin% basename(lf) ) {
      myLog('No Natura 2000 geopackage found. Redownloading...')
      try({ o <- download.file(url = n_link,
                               destfile = paste0(folder,'/', file_g))
      })
      if(class(o)=='try-error' || o != 0){ stop('Natura 2000 file could not be downloaded. Change download link')}
      # Unzip and delete the downloaded file
      unzip(zipfile = lf[ grep(file_g, basename(lf)) ], exdir = folder, overwrite = TRUE)
      file.remove(lf[ grep(file_g, basename(lf)) ])
    }
  }
}

#' Internal unction to prepare the definitions file for further use
#' @param fname File name pointing to the Natura2000_end2019_dataset_definitions.xls file
#' @author Martin Jung
#' @return Saves a list of definitions from the sheet in 'data'
#' @keywords internal

n2000_definitions <- function(fname){
  assertthat::assert_that(file.exists(fname),
                          msg = 'Download and store this file: https://www.eea.europa.eu/data-and-maps/data/natura-11/table-definitions/table-definitions-xls-file/at_download/file')
  require(readxl)

  # Output list
  def <- list()
  for(sh in readxl::excel_sheets(fname) ){
    def[[sh]] <- readxl::read_xls(fname,sheet = sh)
  }

  # Save the output
  dir.create('data',showWarnings = FALSE)
  save('def',file = 'data/NATURA2000_definitions.rdata')
}


#' Show N2000 database definitions
#'
#' @param table Which table of the definitions (optional)
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
