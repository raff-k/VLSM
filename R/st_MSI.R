#' Calculate the mean shape index of a polygons
#'
#' This function calculates the mean shape index of polygons.
#'
#' @param tool tool to perform dissolve operation. Default is \code{"sf"}. However, for complex polygons \code{"grass"} is highly recommended. Fot the use of \code{"grass"} a valid GRASS GIS-session mus be initiated. \code{"saga"} is NOT supported
#' @param x polygon of class \code{sf}
#' @param geom.boundary polygon of class \code{sf} representing subregions, e.g. administrative boundaries. Default: \code{NULL}
#' @param field field name or table column (\code{character}) on which the shannon index is calculated
#' @param do.preProcessing If \code{TRUE} (default), the input of \code{geom.frag} is, first, dissolved by field, and second, splitted to multi-parts. By this step it is assured, that polygon connected to each other are summarized
#' @param return.geom If set to \code{TRUE}, geometries (e.g. dissolved) are returned as well. Default: \code{FALSE}
#' @param quiet If set to \code{FALSE} actual state is printed to console. Default: \code{TRUE}.
#' @note Code is based on the following references:
#' \itemize{
#'   \item Forman, R.T.T., & Godron, M. (1986). Landscape Ecology. Cambridge.
#'   \item Siedentop, S., Heiland, S., Lehmann, I., & Schauerte-Lüke, N. (2007). Regionale Schlüsselindikatoren nachhaltiger Flächennutzung für die Fortschrittsberichte der Nationalen Nachhaltigkeitsstrategie–Flächenziele (Nachhaltigkeitsbarometer Fläche). Abschlussbericht, BBR-Forschungen, H, 130.
#' }
#' @return
#' \code{data.frame} with mean shape index and area weighted mean shape index
#'
#'
#' @keywords simple feature, mean shape index, area weighted mean shape index
#'
#'
#' @export
#'
st_MSI = function(tool = "sf", x, geom.boundary = NULL, field = NULL, do.preProcessing = TRUE, return.geom = FALSE, quiet = TRUE){
  
  # get start time of process
  process.time.start <- proc.time()
  
  ## check validity of geometries
  if(!all(sf::st_is_valid(x))){ stop('Input of "x" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  if(!is.null(geom.boundary) && !all(sf::st_is_valid(geom.boundary))){ stop('Input of "geom.boundary" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  

  if(do.preProcessing && !is.null(field))
  {
    if(!quiet) cat("... dissolve geometries")
    
    if(tool == "sf")
    {
      if(!quiet) cat(" using sf \n")
      x <- x %>% st_dissolve(x = ., by = field) %>% 
      {suppressWarnings(sf::st_collection_extract(x = ., type = "POLYGON"))} %>% 
      {suppressWarnings(sf::st_cast(x = ., to = "POLYGON", warn = FALSE))}
      
    } else if(tool == "grass"){
      if(!quiet) cat(" using grass \n")
      x <- x %>% rgrass_dissolve(x = ., column = field, split = TRUE, check.geom = FALSE, quiet = quiet) %>%
        dplyr::select(-c("cat"))
      
    } else {
      stop('Wrong "tool" selected! Please use either "sf" (default) or "grass"!\n')
    }
  }
  
  x <- x %>% dplyr::mutate(ID_SHAPE = 1:nrow(.)) %>% dplyr::select(c("ID_SHAPE", "geometry"))
  
  if(!is.null(geom.boundary))
  { 
    geom.boundary <- geom.boundary %>% dplyr::mutate(ID_BOUNDS = 1:nrow(.)) %>% 
                                       dplyr::select(c("ID_BOUNDS", "geometry"))
    x <- sf::st_intersection(x = x, y = geom.boundary)
  } # ## add unique IDs
  
  x$SI <- x %>% st_shape_indices(.) %>% .$shape_index
  x$A_m_sq <- x %>% sf::st_area(.) %>% as.numeric(.)
  
  df.x <- x %>% sf::st_drop_geometry(x = .) %>% data.table::as.data.table(.)
  
  if(!is.null(geom.boundary))
  { 
    df.result <- df.x[, list(MSI = mean(x = SI, na.rm = TRUE),
                             AWMSI = stats::weighted.mean(x = SI, w = A_m_sq, na.rm = TRUE)),
                      by = list(ID_BOUNDS)]
                       
  } else {
    df.result = data.table(MSI = mean(x = df.x$SI, na.rm = TRUE),
                           AWMSI = stats::weighted.mean(x = df.x$SI, w = df.x$A_m_sq, na.rm = TRUE))
  }
  
  
  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of st_MSI: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")
  
  if(return.geom)
  {
    return(list(MSI = df.result, geom = x))
  } else {
    return(list(MSI = df.result))
  }
} # end of function st_MSI
