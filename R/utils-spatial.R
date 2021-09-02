#' Get administrative boundaries for European countries
#'
#' @description Uses the \code{rnaturalearth} package to retrieve global administrative boundaries, which are reprojected, and then clipped to a bounding box around Europe.
#' @param scale (optional) The \code{numeric} scale to pass to \code{rnaturalearth::ne_countries()}. Acceptable values are \code{10} (highres), \code{50} (midres), \code{110} (lowres). Defaults to \code{110}.
#' @param bbox (optional) A \code{sf} object from which a bounding box can be retrieved to clip the countries boundaries. Defaults to the bounding box of the \code{europe_100km} grid included in the \code{rN2000} package.
#' @param crs (optional) A \code{crs}, \code{sf}, or \code{numeric} object giving a CRS to reproject the data to. If \code{crs} is numeric, this should be an EPSG code. Defaults to \code{ETRS89-extended / LAEA Europe}.
#' @param ... other parameters passed to \code{rnaturalearth::ne_countries()}
#' @return A \code{sf} object with the administrative boundaries of the relevant European countries.
#' @examples
#'\dontrun{
#' get_EuropeanCountries()
#' }
#' @keywords internal
#' @author Matt Lewis

get_EuropeanCountries <-
  function(scale = 110, bbox = europe_100km, crs = sf::st_crs(europe_100km), ...){
    assertthat::assert_that(inherits(scale, "numeric"),
                            scale %in% c(10, 50, 110),
                            msg = "The provided input for scale is not valid. Try `10`, `50`, or `110`.")
    assertthat::assert_that(inherits(bbox, c("sf", "sfc")),
                            msg = "Please provide a valid `sf` object for `bbox`.")
    assertthat::assert_that(inherits(crs, c("crs", "sf", "sfc", "numeric")),
                            msg = "Please provide a valid input for `crs`.")
    assertthat::assert_that(requireNamespace("rgeos", quietly = T),
                            msg = "The `rgeos` package is required for this function.
                                    Please first install it with `install.packages('rgeos')`.")
    if(scale == 110 &&
       requireNamespace("rnaturalearthdata", quietly = T) != T){
      stop("Retrieving country boundaries at `scale = 110` requires the `rnaturalearthdata` package.
            Please first install it with `install.packages('rnaturalearthdata')`.")
    }else if(scale == 10 &&
             requireNamespace("rnaturalearthhires", quietly = T) != T){
      stop("Retrieving country boundaries at `scale = 10` requires the `rnaturalearthhires` package.
            Please first install it with `remotes::install_github('https://github.com/ropensci/rnaturalearthhires')`.")
    }

    bbox <-
      bbox %>%
      sf::st_bbox() %>%
      sf::st_as_sfc()

    if(sf::st_crs(bbox) != sf::st_crs(crs)){
      bbox <-
        bbox %>%
        sf::st_transform(sf::st_crs(crs))
    }

    countries <-
      rnaturalearth::ne_countries(scale = scale, type = "countries", returnclass = "sf", ...) %>%
      sf::st_transform(sf::st_crs(crs)) %>%
      sf::st_intersection(bbox)

    return(countries)
  }
