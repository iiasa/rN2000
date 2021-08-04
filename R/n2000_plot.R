#' Plot Natura 2000 data
#' @description Wrapper around \code{ggplot} for convenience plotting with optional grid or administrative boundaries. Requires \code{ggplot2} to be installed.
#' @param x A \code{sf} object to plot.
#' @param option The \code{character} type of data to plot. Options are \code{"PO"} ('presence only' - sites where the species is present), \code{"PA"} ('presence absence' - sites where the species is present and absent), \code{"abundancelow"} (the lower bound of abundance), \code{"abundancehigh"} (the higher bound of abundance), or \code{"abundancemid"} (the arithmetic mean of the upper and lower population bounds).
#' @param borders (optional) A \code{character} input - should country borders be plotted? Options are \code{"no"}, \code{"lowres"}, \code{"midres"}, \code{"highres"}. Defaults to \code{"midres"}, which, along with \code{lowres} is included in the package data. \code{"highres"} is first retrieved via the \code{rnaturalearth} package.
#' @param grid (optional) A \code{logical} input. Should a grid be added too? Defaults to \code{FALSE}.
#' @param sea (optional) A \code{logical} input. Should the sea be coloured in? Defaults to \code{TRUE}.
#' @author Matt Lewis
#' @export

n2000_plot <-
  function(
    x,
    option,
    borders = "midres",
    grid = F,
    sea = T
  ){
    assertthat::assert_that(requireNamespace("ggplot2", quietly = T),
                            msg = "The `ggplot2` package is required for this function. Please install it.")
    assertthat::assert_that(is.character(borders),
                            is.logical(grid),
                            is.logical(sea),
                            inherits(x, c("sf", "sfc")),
                            borders %in% c("no", "lowres", "midres", "highres"),
                            is.character(option),
                            option %in% c("PO", "PA", paste0("abundance", c("low", "mid", "high")))
                            )
    if(option == "PA"){
      assertthat::assert_that("PA" %in% colnames(x))
    }else if(option %in% c("abundancelow", "abundancehigh")){
      assertthat::assert_that(any(c("LOWERBOUND", "UPPERBOUND") %in% colnames(x)))
    }else if(option == "abundancemid"){
      assertthat::assert_that("AVGPOP" %in% colnames(x))
    }

    ret <-
      ggplot2::ggplot() +
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_line(colour = "gray"))
    if(sea == T){
      ret <-
        ret +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#E6F7FF"))
    }
    if(borders != "no"){
      if(borders == "lowres"){
        ret <-
          ret +
          ggplot2::geom_sf(data = europe_countries_lowres, fill = "gray90")
      }else if(borders == "midres"){
        ret <-
          ret +
          ggplot2::geom_sf(data = europe_countries_midres, fill = "gray90")
      }else{
        eu <-
          get_EuropeanCountries(scale = 10)

        ret <-
          ret +
          ggplot2::geom_sf(data = eu, fill = "gray90")
      }

    }

    if(grid == T){
      ret <-
        ret +
        ggplot2::geom_sf(data = europe_100km, fill = NA)
    }

    bbox <-
      x %>%
      sf::st_bbox()

    if(option == "PO"){
      ret <-
        ret +
        ggplot2::geom_sf(data = x, fill = "darkgreen", colour = "darkgreen")
    }else if(option == "PA"){
      ret <-
        ret +
        ggplot2::geom_sf(data = x, ggplot2::aes(fill = PA, colour = PA))+
        ggplot2::scale_color_manual(values = c("red", "darkblue")) +
        ggplot2::Scale_fill_manual(values = c("red", "darkblue"))
    }else if(option %in% paste0("abundance", c("low", "mid", "high"))){
      if(option == "abundancelow"){
        ret <-
          ret +
          ggplot2::geom_sf(data = x,
                           ggplot2::aes(fill = LOWERBOUND,
                                        color = LOWERBOUND))
      }else if(option == "abundancemid"){
        ret <-
          ret +
          ggplot2::geom_sf(data = x,
                           ggplot2::aes(fill = AVGPOP,
                                        color = AVGPOP))
      }else{
        ret <-
          ret +
          ggplot2::geom_sf(data = x,
                           ggplot2::aes(fill = UPPERBOUND,
                                        color = UPPERBOUND))
      }
      ret <-
        ret +
        ggplot2::scale_color_viridis_b(option = "magma") +
        ggplot2::scale_fill_viridis_b(option = "magma")
    }


    ret <-
      ret +
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand =T)


    plot(ret)
  }
