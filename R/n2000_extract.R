#' Extract information from Natura 2000 sites within a target area of interest.
#'
#' @description Extract Natura 2000 site information which overlap specified spatial locations.
#' @param n2000_file The file path to the Natura2000 geopackage. Alternatively the name of the loaded geopackage in the environment.
#' @param target_areas The file path to the spatial polygon(s) for the area of interest. Must be readable by \code{sf::st_read()}. Alternatively the name of the loaded spatial data in the environment.
#' @param byid Logical. Should data be extracted for each target area (TRUE) or should they be merged first (FALSE)? Defaults to TRUE.
#' @param enclosed Logical. Should Natura 2000 sites only be included if they are fully enclosed by the target area? Defaults to FALSE.

#' @details If the CRS of the \code{target_areas} does not match that of the Natura 2000 data, the \code{target_areas} are first reprojected.
#' @export
#' @author Matt Lewis

n2000_extract <- function(n2000_file,
                         target_areas,
                         byid = T,
                         enclosed = F){
  # load spatial data
  n2000_data <-
    ifelse(
      is.character(n2000_file),
      sf::st_read(n2000_file),
      n2000_file
    )

  target_areas <-
    ifelse(
      is.character(target_areas),
      sf::st_read(target_areas),
      target_areas
    )

  # Check
  assertthat::assert_that(is.logical(byid),
                          is.logical(enclosed))

  # union target areas if wanted
  if(byid == F){
    target_areas <-
      target_areas %>%
      sf::st_union()
    target_areas$target_ID <- 1
  }

  # Check and reproject target areas
  if(sf::st_crs(target_areas) != sf::st_crs(n2000_data)){
    target_areas <-
      target_areas %>%
      st_transform(
        crs = st_crs(n2000_data)
      )
  }

  # Choose extraction function
  if(enclosed == T){
    extr_function <- sf::st_within()
  }else{
    extr_function <- sf::st_intersects()
  }

  # Extract info
  output_df <-
    cbind(
      as.data.frame(target_areas[1,]),
      as.data.frame(n2000_data[1,])
    )

  for(i in 1:nrow(target_areas)){
    tmp <-
      target_areas[i,] %>%
      extr_function(n2000_data) %>%
      unlist()
    if(length(tmp) >=1L){
      tmp_df <- output_df[rep(1,length(tmp)),]
      tmp_df[,1:length(target_areas)] <- target_areas[i,]
      tmp_df[,(1+length(target_areas)):length(tmp_df)] <- n2000_data[tmp,]
    }else{
      tmp_df <- output_df[1,]
      tmp_df[,1:length(target_areas)] <- target_areas[i,]
      tmp_df[,(1+length(target_areas)):length(tmp_df)] <- NA
    }


    output_df <-
      ifelse(
        i == 1,
        tmp_df,
        rbind(
          output_df,
          tmp_df
        )
      )
  }

  return(output_df)
}
