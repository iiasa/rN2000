#' Plots retrieved Natura 2000 with optional grid, and country borders
#' @description Wrapper around ggplot for convenience. Requires `ggplot2` to be installed.
#' @param x A \code{sf} object to plot.
#' @param borders A \code{logical} input - should country borders be plotted too? Defaults to \code{FALSE}.
#' @param grid A \code{logical} input. Should a grid be added too? Defaults to \code{FALSE}.
#' @param sea A \code{logical} input. Should the sea be coloured in? Defaults to \code{TRUE}.
#' @author Matt Lewis
#' @export

n2000_plot <-
  function(
    x,
    borders = T,
    grid = F,
    sea = T
  ){
    assertthat::assert_that(requireNamespace("ggplot2", quietly = T),
                            msg = "The `ggplot2` package is required for this function. Please install it.")
    assertthat::assert_that(is.logical(borders),
                            is.logical(grid),
                            is.logical(sea),
                            inherits(x, c("sf", "sfc"))
                            )

    ret <-
      ggplot2::ggplot() +
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid = ggplot2::element_line(colour = "gray"))
    if(sea == T){
      ret <-
        ret +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#E6F7FF"))
    }
    if(borders == T){
      ret <-
        ret +
        ggplot2::geom_sf(data = europe_countries, fill = "gray90")
    }

    if(grid == T){
      ret <-
        ret +
        ggplot2::geom_sf(data = europe_100km, fill = NA)
    }

    bbox <-
      x %>%
      sf::st_bbox()

    ret <-
      ret +
      ggplot2::geom_sf(data = x, fill = "darkgreen", colour = "darkgreen")+
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand =T)


    plot(ret)
  }
