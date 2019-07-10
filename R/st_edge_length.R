#' Calculate edge length between two geometries
#'
#' This function calculates the edge length between two geometries.
#'
#' @param x polygon of class \code{sf} representing the basis geometry
#' @param y polygon of class \code{sf} representing geometry of which the edge length is calculated
#' @param dist.x buffer distance in mapping units around geometry using \code{sf::st_buffer}. Default: \code{0} 
#' @param dist.y buffer distance in mapping units around geometry using \code{sf::st_buffer}. Default: \code{0} 
#' @param do.preProcessing If \code{TRUE} (default), the input of \code{geom.frag} is, first, dissolved to single part feature, and second, splitted to multi-parts
#' @param return.geom If set to \code{TRUE}, intermediate geometries are returned as well. Default: \code{FALSE}
#' @param quiet If set to \code{FALSE}, actual state is printed to console. Default: \code{TRUE}.
#' @note If the edge line has incomprehensible gabs(can happen for "complicated" geometries), it can help to use a small buffer distance (< 1) to get a complete edge line.
#' @return
#'  \code{list} with edge length, and if \code{return.geom} is \code{TRUE} also the edge-length-geometry (and eventually buffer results).
#'
#'
#' @keywords edge length
#'
#'
#' @export
#'
st_edge_length = function(x, y, dist.x = 0, dist.y = 0, do.preProcessing = TRUE, return.geom = FALSE, quiet = TRUE)
{
  
  # get start time of process
  process.time.start <- proc.time()
  result.list <- list()
  
  if(do.preProcessing)
  {
    if(!quiet) cat("... union geometries to a single-parts geometry with resolved boundaries \n")
    if(!quiet) cat("... ... x-geometry \n")
    x <- x %>% preProcessing(x = ., split = TRUE) %>% dplyr::rename("ID_x" = "ID")
    
    if(!quiet) cat("... ... y-geometry \n")
    y <- y %>% preProcessing(x = ., split = TRUE) %>% dplyr::rename("ID_y" = "ID")
  }
    
  if(dist.y > 0)
  {
    if(!quiet) cat("... buffer y-geometry \n")
     y <- y %>% sf::st_buffer(x = ., dist = dist.y) %>% preProcessing(x = ., split = TRUE) %>% dplyr::rename("ID_y" = "ID")
     if(return.geom){ result.list$buf_y <- y }
  }
  
  if(dist.x > 0)
  {
    if(!quiet) cat("... buffer x-geometry \n")
    x <- x %>% sf::st_buffer(x = ., dist = dist.x) %>% preProcessing(x = ., split = TRUE) %>% dplyr::rename("ID_x" = "ID")
    if(return.geom){ result.list$buf_x <- x }
  }
  
  # # # START CALCULATION
  if(!quiet) cat("... convert y-geometry to lines \n")
  y.line <- y %>% sf::st_cast(x = ., to = "POLYGON", warn = FALSE) %>% sf::st_cast(x = ., to = "LINESTRING", warn = FALSE)
  
  if(!quiet) cat("... get edges: Intersection of y-lines with x-geometry \n")
  y.line.inter <- suppressWarnings(sf::st_intersection(x = y.line, y = x)) %>%
                    sf::st_collection_extract(., "LINESTRING", warn = FALSE) 
  # WARN:  sf::st_collection_extract(...) can lead to unwished results, see https://github.com/r-spatial/sf/issues/913
  
  if(return.geom){ result.list$geom_edge <- y.line.inter }
  
  if(!quiet) cat("... calculate edge length \n")
  result.list$edge_length <- sf::st_length(y.line.inter) %>% as.numeric(.) %>% sum(., na.rm = TRUE)
  
  # re-order list
  if(return.geom){ result.list <- result.list[rev(names(result.list))]}
  
  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of st_edge_length: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")

  return(result.list)

} # end of function st_edge_length