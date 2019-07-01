#' Calculate the shape indices of a polygon
#'
#' This function calculates the shape index of a polygon.
#'
#' @param x object of class \code{sf} 
#' @return
#' \code{List} with shape index and interior edge ratio values
#'
#'
#' @keywords simple feature, shape index, interior edge ratio
#'
#'
#' @export
#'
st_shape_indices = function(x){
  perimeter <- x %>% st_perimeter(.)
  area <- x %>% sf::st_area(.) %>% as.numeric(.)
  
  shape_index <- perimeter / (2 * sqrt(pi * area))
  interior_edge_ratio <- perimeter / area
  
  return(list(shape_index = shape_index, interior_edge_ratio = interior_edge_ratio))
}
