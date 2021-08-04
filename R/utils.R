#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom methods as
NULL

#' @importFrom utils unzip download.file read.csv
NULL

#' Inverse of in call for convenience
#' Calculates the set of entries not present in the second vector
#'
#' @param a First vector
#' @param b Second vector
#' @keywords internal
#' @noRd
#' @author Martin Jung

`%notin%` = function(a, b){!(a %in% b)}

#' Custom logging function for scripts
#'
#' @param title The title in the log output
#' @param ... Any additional outputs or words for display
#' @keywords internal
#' @noRd
#' @author Martin Jung

myLog <- function(title = "[Processing]",...) {
  cat(paste0(title,' ', Sys.time(), " | ", ..., "\n"))
}


#' Write raster files as GeoTiffs with useful compression options
#'
#' @param x A \code{raster} object.
#' @param file A \code{character} file path and file name (e.g. \code{'filepath/myraster.tif'}).
#' @param datatype A \code{character} input for data type. Defaults to \code{'FLT4S'}, but \code{'LOG1S'} is useful for binary rasters. See \code{?raster::dataType()}
#' @param ... Other parameters passed on to \code{raster::writeRaster()}
#' @details Defaults to using the following parameters:
#' \code{NAflag = -9999}; \code{options = c("COMPRESS=DEFLATE","PREDICTOR=2", "ZLEVEL=9")}; \code{format = "GTiff"}; \code{overwrite = T}
#' @keywords internal
#' @author Martin Jung
#' @author Matt Lewis
writeGeoTiff <- function(x, file,datatype = "FLT4S"){
  assertthat::assert_that(
    is.character(file),
    is.character(datatype),
    inherits(x, c("RasterLayer", "RasterBrick", "RasterStack"))
  )

  if(!assertthat::has_extension(file,"tif")){file <- paste0(file,".tif")}

  raster::writeRaster(
    x,
    file,
    format='GTiff',
    datatype = datatype,
    NAflag = -9999,
    options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),
    overwrite= TRUE
    )
}
