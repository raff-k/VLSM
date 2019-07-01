#' Erase one geometry from another
#'
#' This function erases one geometry from another. The projection must be identical.
#'
#' @param x object of class \code{sf}
#' @param y object of class \code{sf}
#' @param precision see \link[sf]{st_set_precision}. Default: \code{0}
#' @param do.subset Perform \link[sf]{st_intersects} to subset geometry of \code{x}.Default: \code{TRUE}
#' @importFrom magrittr "%>%"
#' @return
#' Geometry of class \code{sfc}
#'
#'
#' @keywords simple feature, erase
#'
#'
#' @export
#'
st_erase = function(x, y, precision = 0, do.subset = TRUE)
{
  if(do.subset)
  {
    inter <- sf::st_intersects(x = y, y = x)  %>% unlist(.) %>% unique(.)
    x.remain <- x[-inter,]
    x <- x[inter,]
  }
  
  if(precision != 0)
  {
    x <- x %>% sf::st_set_precision(x = ., precision = precision) %>% lwgeom::st_make_valid(.)
    y <- y %>% sf::st_combine(.) %>% sf::st_union(.) %>%
      sf::st_set_precision(x = ., precision = precision) %>% lwgeom::st_make_valid(.)
  }
  
  out <- sf::st_difference(x = x, y = y) # erase y from x
  
  if(do.subset)
  {
    out <- rbind(out, x.remain)
  }
  
  return(out)
}



#' Erase one geometry from another using Saga GIS
#'
#' This function erase one geometry from another. The projection must be identical.
#'
#' @param x object of class \code{sf}. First element: Should be either of type line or polygon
#' @param y object of class \code{sf}. Second element: Always of type polygon.
#' @param method method of erase. Either \code{"1"}: Polygon-Polygon-Erase , or \code{"2"}: Line-Polygon-Erase. Default: \code{"1"}
#' @param split Set to \code{"1"}, if multi-part polygons should be splitted to single-part polygons. Default: \code{"0"}
#' @param attributes attributes inherited to intersection result. \code{0} polygon, \code{1} line, \code{2} line and polygon. Default: \code{"1"}
#' @param env.rsaga SAGA GIS environemnt. Default: \link[RSAGA]{rsaga.env}
#' @param check.geom If set to  \code{TRUE} then geometry is checked with \link[sf]{st_is_valid}. If there are invalid geometries, geometries are repaired using \link[lwgeom]{st_make_valid}. Default: \code{TRUE}
#' @param quiet If \code{FALSE} then comments are printed. Default: \code{TRUE}
#' @importFrom magrittr "%>%"
#' @return
#' Geometry of class \code{sfc}
#'
#'
#' @keywords simple feature, erase
#'
#'
#' @export
#'
rsaga_erase = function(x, y, method = "1", split = "0", attributes = "1", env.rsaga = RSAGA::rsaga.env(), check.geom = TRUE, quiet = TRUE)
{
  path.x <- file.path(tempdir(), "tmp_x.shp")
  path.y <- file.path(tempdir(), "tmp_y.shp")
  path.result <- file.path(tempdir(), "tmp_result.shp")
  
  sf::st_write(obj = x, dsn = path.x, delete_layer = TRUE, quiet = quiet)
  sf::st_write(obj = y, dsn = path.y, delete_layer = TRUE, quiet = quiet)
  
  if(method == "1")
  {
    # RSAGA::rsaga.get.usage(lib = "shapes_polygons", module = 15, env = env.rsaga)
    RSAGA::rsaga.geoprocessor(lib = "shapes_polygons", module = 15, env = env.rsaga, show.output.on.console = !quiet, param = list(
      A = path.x, B = path.y, RESULT = path.result, SPLIT = split))
  }
  
  if(method == "2")
  {
    # RSAGA::rsaga.get.usage(lib = "shapes_lines", module = 3, env = env.rsaga)
    # ATTRIBUTES: [1] line
    RSAGA::rsaga.geoprocessor(lib = "shapes_lines", module = 3, env = env.rsaga, show.output.on.console = !quiet, param = list(
      LINES = path.x, POLYGONS = path.y, ATTRIBUTES = attributes, DIFFERENCE = path.result))
  }
  
  ## read data
  out <- sf::st_read(dsn = path.result, quiet = quiet)
  
  ## check validity
  if(check.geom && !all(sf::st_is_valid(out)))
  {
    warning('Some invalid geometries by "rsaga_erase". Try to correct geomeries using lwgeom::st_make_valid()!')
    out <- lwgeom::st_make_valid(x = out)
    
    if(method == "1")
    {
      out <- suppressWarnings(out %>% sf::st_collection_extract(x = ., type = c("POLYGON")))
    }
    
    if(method == "2")
    {
      out <- suppressWarnings(out %>%  sf::st_collection_extract(x = ., type = c("LINESTRING")))
    }
  }
  return(out)
}





#' Return geometry from bounding box
#'
#' This function calculate the geometry based on a bounding box.
#'
#' @param x object of class \code{sf}
#' @param extent vector containing numeric values, in the following order: xmin, xmax, ymax, ymin
#' @importFrom magrittr "%>%"
#' @return
#' Geometry of class \code{sfc}
#'
#'
#' @keywords simple feature, geometry of bounding box
#'
#'
#' @export
#'
st_bbox_geom = function(x, extent = NULL)
{
  if(!is.null(x) & !is.null(extent))
  {
    stop("Input conflict: either x or extent!")
  }
  
  if(is.null(extent))
  {
    out <- sf::st_bbox(x) %>% sf::st_as_sfc(.)
  } else {
    names(extent) <- c("xmin", "xmax", "ymax", "ymin")
    out <- sf::st_bbox(extent) %>% sf::st_as_sfc(.)
  }
  return(out)
} # end of function st_bbox_geom




#' Dissolve geometry
#'
#' This function dissovle a geometry based on a field. Optionnally, field statistics can be computed.
#'
#' @param x object of class \code{sf}
#' @param by field for dissolving. Default: \code{NULL}
#' @param ... Optional: field statistcs
#' @importFrom magrittr "%>%"
#' @return
#' dissolved geometry of class \code{sf}
#'
#'
#' @keywords simple feature, dissolve
#'
#'
#' @export
#'
st_dissolve = function(x, by = NULL, ...) x %>% dplyr::group_by(.dots = by) %>% dplyr::summarise(...)


#' Calculate the perimeter of a polygon
#'
#' This function calculates the perimeter of a polygon.
#'
#' @param x object of class \code{sf}
#' @param ... Optional parameters for function link[sf]{st_cast}
#' @importFrom magrittr "%>%"
#' @return
#' Vector with perimeter values
#'
#'
#' @keywords simple feature, perimeter
#'
#'
#' @export
#'
st_perimeter = function(x, ...) suppressWarnings(x %>% sf::st_cast(x = ., to = "MULTILINESTRING", ...) %>% sf::st_length(.) %>% as.numeric(.))




#' Make multiline fishnet
#'
#' This function creates a fishnet, consisting of multilines in x and y direction.
#'
#' @param x object of class \code{sf}
#' @param cellsize cell size of fishnet grid in meter
#' @importFrom magrittr "%>%"
#' @return
#' Geometry of class \code{sfc}
#'
#'
#' @keywords simple feature, fishnet, multilines
#'
#'
#' @export
#'
st_make_grid_lines = function(x, cellsize)
{
  # ... create fishnet using corners (lower left corner is starting point)
  fishnet <- sf::st_make_grid(x = x, cellsize = cellsize, what = "corners")
  
  # ... get sf::st_make_grid function parameters (automatically calculated in function call)
  offset <- sf::st_bbox(obj =  x)[1:2]
  nx <- ceiling((sf::st_bbox(obj = x)[3] - offset[1])/cellsize[1]) + 1 # add 1 to fit overall columns
  ny <- ceiling((sf::st_bbox(obj = x)[4] - offset[2])/cellsize[2]) + 1 # add 1 to fit overall rows
  
  # ... create lines
  linesX <- lapply(1:ny, function(i, nx, fishnet){
    if(i == 1)
    {
      index <- 1:nx
    } else {
      index <- ((i-1)*nx+1):(i*nx)
    }
    
    line <- fishnet[index] %>% st_combine(.) %>%
      sf::st_multilinestring(x = ., dim = "XY") %>%
      st_sfc(.) %>%
      sf::st_sf(ID_FNET = paste0("X", i), geometry = .)
    
    return(line)
  }, nx = nx, fishnet = fishnet) %>% do.call(what = rbind, args = .)
  
  seqY <- seq(from = 1, to = length(fishnet), by = nx)
  linesY <- lapply(0:(nx-1), FUN = function(i, seqY, fishnet){
    
    if(i == 0){
      index <- seqY
    } else {
      index <- seqY+i
    }
    
    line <- fishnet[index] %>% st_combine(.) %>%
      sf::st_multilinestring(x = ., dim = "XY") %>%
      st_sfc(.) %>%
      sf::st_sf(ID_FNET = paste0("Y", i), geometry = .)
    
    return(line)
  }, seqY = seqY, fishnet) %>% do.call(what = rbind, args = .)
  
  
  
  fishnet.multi <- rbind(linesX, linesY)
  sf::st_crs(fishnet.multi) <- sf::st_crs(fishnet)
  
  if(!all(sf::st_is_valid(fishnet.multi)))
  {
    warning("Fishnet contains invalid geometries!")
  }
  
  return(fishnet.multi)
} # end of function st_make_grid_lines
