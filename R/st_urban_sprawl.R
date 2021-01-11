#' Calculate urban sprawl
#'
#' This function calculates the urban sprawl of urban area.
#'
#' @param tool tool to perform erase-vector-operation. Default is \code{"sf"}. However, for complex polygons \code{"grass"} is highly recommended. Fot the use of \code{"grass"} a valid GRASS GIS-session mus be initiated. \code{"saga"} is also supported, then \code{env.rsaga} must be properly set.
#' @param geom.urban polygon of class \code{sf} representing the fragmentation geometry
#' @param geom.boundary polygon of class \code{sf} representing subregions, e.g. administrative boundaries
#' @param dist \code{vector} containing distance between lines in \code{x} and \code{y} direction. Default: \code{c(100, 100)} \code{m}
#' @param trans transformation function \code{x-1+1/(x+trans.k)} with \code{x} as free line and \code{trans.k} as constant
#' @param trans.k constant in \code{km} for transformation function \code{trans}. Default: \code{1}
#' @param tol tolerance value for intersection with erased lines. Buffering procedure is used. Default: \code{0.1} \code{m}
#' @param precision precision for process \code{sf::st_set_precision}. See \link[sf]{st_precision}. Default: \code{0}
#' @param extent Numeric value representing extent for area. Format of vector: \code{c(xmin, xmax, ymax, ymin)}. Default: \code{NULL}
#' @param force.extent If \code{TRUE} extent is used instead of \code{geom.boundary} (if both are present). Default: \code{FALSE}
#' @param override If \code{TRUE} projection of geometry-operation is replaced by projection of \code{geom.urban}. This flag prevents errors when the original datum is not found by the \code{tool}. Dafault: \code{FALSE} 
#' @param do.preProcessing If \code{TRUE} (default), the input of \code{geom.frag} is, first, dissolved to single part feature, and second, splitted to multi-parts. By this step it is assured, that polygon connected to each other are summarized
#' @param return.geom If set to \code{TRUE}, intermediate geometries are returned as well. Default: \code{FALSE}
#' @param env.rsaga environment of \code{SAGA GIS}. If \code{st_erase} fails then \code{SAGA GIS erase} is used. Default: \code{NULL}, but in function call if not set: \link[RSAGA]{rsaga.env}
#' @param quiet If set to \code{FALSE}, actual state is printed to console. Default: \code{TRUE}.
#' @note Code is based on the following references:
#' \itemize{
#'   \item Ackermann, W., Schweiger, M., Sukopp, U., Fuchs, D., & Sachteleben, J. (2013). Indikatoren zur biologischen vielfalt: Landschaftszersiedlung (Biodiversity indicators. Development and accounting). Naturschutz und Biologische Vielfalt, 132.
#' }
#' Depending on the selected \code{tool}, the result of the vector-operation can differ significantly!
#' @return
#'  strong urban sprawl: 40-50%, less urban sprawl: 80-90%
#'
#'
#' @keywords simple feature, urban sprawl
#'
#'
#' @export
#'
st_urban_sprawl = function(tool = "sf", geom.urban, geom.boundary = NULL, dist = c(100, 100), trans = function(x, trans.k){x-1+1/(x+trans.k)}, trans.k = 1, tol = 0.1, extent = NULL, force.extent = FALSE,
                           override = FALSE, precision = 0, do.preProcessing = TRUE,  return.geom = FALSE, env.rsaga = NULL, quiet = TRUE)
{
  # get start time of process
  process.time.start <- proc.time()
  
  ## check input
  if(missing(geom.urban)){ stop('Input of "geom.urban" is missing!')}
  if(!missing(geom.urban) && !("sf" %in% class(geom.urban))){ stop('Input of "geom.urban" is not of class "sf"!')}
  if(!is.null(geom.boundary) && !("sf" %in% class(geom.boundary))){ stop('Input of "geom.boundary" is not of class "sf"!')}
  if(is.null(geom.boundary) && is.null(extent)){
    warning('If input of "geom.boundary" and "extent" is null. Fish net is created using the extent of "geom.urban"!')
  }
  if(is.null(geom.boundary) & is.null(extent) & force.extent){
    stop('If "force.extent" is TRUE, than extent should be set!"!')
  }
  if(!is.null(geom.boundary) & !is.null(extent)){
    warning('Fish net is created using extent of "geom.boundary". "Extent" is skipped. For "extent" use "force.extent"!')
  }
  
  if(sf::st_precision(geom.urban) != precision)
  {
    geom.urban <- geom.urban %>% sf::st_set_precision(x = ., precision = precision) %>%
      sf::st_make_valid(.) %>%
      sf::st_collection_extract(x = ., type = "POLYGON")
  }
  
  
  ## check validity of geometries
  if(!all(sf::st_is_valid(geom.urban))){ stop('Input of "geom.urban" contains not valid geometries. Please try sf::st_make_valid().')}
  if(!is.null(geom.boundary) && !all(sf::st_is_valid(geom.boundary))){ stop('Input of "geom.boundary" contains not valid geometries. Please try sf::st_make_valid().')}
  
  ## add unique ID and subset data
  if(!is.null(geom.boundary)){ geom.boundary$ID_BNDS <- 1:nrow(geom.boundary) } # ## add unique IDs
  if(!is.null(geom.boundary)){geom.boundary <- geom.boundary[, c("ID_BNDS", "geometry")]}
  
  
  ## ... create boundary for fishnet
  if(!is.null(geom.boundary) & !force.extent){bbox.fishnet <- st_bbox_geom(x = geom.boundary)
  } else if(!is.null(geom.boundary) & force.extent){bbox.fishnet <- st_bbox_geom(extent = extent)
  } else if(is.null(geom.boundary) & !is.null(extent)){bbox.fishnet <- st_bbox_geom(extent = extent)
  } else { bbox.fishnet <- st_bbox_geom(x = geom.urban) }
  
  
  ## create and subset fish net
  if(!quiet) cat("... create fishnet \n")
  fishnet <- st_make_grid_lines(x = bbox.fishnet, cellsize = dist)
  
  if(!is.null(geom.boundary) & !force.extent){
    if(!quiet) cat("... subset fishnet to area of interest \n")
    fishnet <- suppressWarnings(sf::st_intersection(x = fishnet, y = geom.boundary)) %>%
      sf::st_collection_extract(x = ., type = "LINESTRING", warn = FALSE)
  } else {
    fishnet <- suppressWarnings(sf::st_intersection(x = fishnet, y = bbox.fishnet)) %>%
      sf::st_collection_extract(x = ., type = "LINESTRING", warn = FALSE)
  }
  

  ## do post-processing of urban geometry
  if(do.preProcessing)
  {
    if(!quiet) cat("... union geometries to a single-parts geometry with resolved boundaries \n")
    geom.urban <- geom.urban %>% preProcessing(x = ., split = TRUE) %>% 
                                dplyr::rename("ID_URBAN" = "ID")
  } else {
    geom.urban$ID_URBAN <- 1:nrow(geom.urban) ## add unique IDs
  }
  
  ## check intersection
  inter.check <- sf::st_intersects(x = geom.urban, y = fishnet)
  inter.empty <- which(sapply(inter.check, function(x) length(x) == 0))
  if(length(inter.empty) > 0)
  {
    inter.A <- geom.urban[inter.empty,] %>% sf::st_area(.) %>% as.numeric(.)
    inter.num <- length(inter.empty)
    
    warning('Some urban polygons are not intersected by fishnet: ', inter.num,
            ' | area min: ', min(inter.A, na.rm = TRUE), ' - max: ', max(inter.A, na.rm = TRUE), ' [m_sqr] \n')
  }
  
  
  ## erase urban area from fishnet using SAGA GIS
  if(!quiet) cat("... erase urban area from fishnet \n")
  
  if(tool == "saga")
  {
    if(is.null(env.rsaga))
    {
      env.rsaga <-  RSAGA::rsaga.env()
    }
    erase <-  rsaga_erase(x = fishnet, y = geom.urban, method = "2", env.rsaga = env.rsaga)
  } else if(tool == "grass"){
    erase <-  rgrass_overlay(x = fishnet, y = geom.urban, operator = "not") # not: also known as 'difference'
    erase <- erase %>% dplyr::select(- c("ID_URBAN"))
  } else if(tool == "sf"){
    erase <- suppressWarnings(st_erase(x = fishnet, y = geom.urban, precision = precision) %>%
                                sf::st_collection_extract(x = ., type = "LINESTRING"))
  } else {
    stop("No valid input tool! \n")
  }
  
  if(override)
  {
    if(!quiet) cat("... override projection \n")
    erase <- suppressWarnings(erase %>% sf::st_set_crs(., value = geom.urban %>% sf::st_crs(.)))
  }
  
  ## split to single part
  erase.single <- erase %>% sf::st_cast(x = ., to = 'LINESTRING', warn = FALSE)
  
  
  ## selection of lines
  if(!quiet) cat("... selection of lines \n")
  lines.urban <- sf::st_intersects(x = geom.urban %>%
                                     sf::st_buffer(x = ., dist = tol),
                                   y = erase.single) %>%
    unlist(.) %>% unique(.)
  
  
  if(!is.null(geom.boundary) & !force.extent)
  {
    lines.boundary <- sf::st_intersects(x = geom.boundary %>%
                                          sf::st_boundary(x = .) %>%
                                          sf::st_buffer(x = ., dist = tol),
                                        y = erase.single) %>%
      unlist(.) %>% unique(.)
  } else {
    lines.boundary <- sf::st_intersects(x = bbox.fishnet %>%
                                          sf::st_boundary(x = .) %>%
                                          sf::st_buffer(x = ., dist = tol),
                                        y = erase.single) %>%
      unlist(.) %>% unique(.)
  }
  
  check.missing <- setdiff(c(1:nrow(erase.single)),
                           (c(lines.urban, lines.boundary)  %>% unique(.)))
  
  if(length(check.missing) > 0)
  {
    warning("Some lines are neither intersected by boundary nor by urban area: ", length(check.missing), " in total \n")
  }
  
  erase.single$Type <- NA
  
  # TPYE 1: Lines between urban area (red lines)
  type.urban <- setdiff(x = lines.urban, y = lines.boundary)
  erase.single$Type[type.urban] <- 1
  
  # TPYE 2: Lines not touching urban area (blue lines)
  type.boundary <- setdiff(x = lines.boundary, y = lines.urban)
  erase.single$Type[type.boundary] <- 2
  
  # TYPE 3: Lines between boundary and urban area (green lines)
  type.bound.urb <- intersect(x = lines.urban, y = lines.boundary)
  erase.single$Type[type.bound.urb] <- 3
  
  ## group single lines back to multi-lines
  if((!is.null(geom.boundary) & !force.extent))
  {
    erase.final <- st_dissolve(x = erase.single[which(erase.single$Type == 2 | erase.single$Type == 3),],
                               by = list("ID_BNDS", "Type", "ID_FNET"))
    
    erase.final <- rbind(erase.final, erase.single[which(erase.single$Type == 1),])
    
  } else {
    erase.final <- st_dissolve(x = erase.single[which(erase.single$Type == 2 | erase.single$Type == 3),],
                               by = list("Type", "ID_FNET"))
    
    erase.final <- rbind(erase.final, erase.single[which(erase.single$Type == 1),])
  }
  
  erase.final <- erase.final %>% sf::st_collection_extract(x = ., type = "LINESTRING")
  
  erase.final$L <- sf::st_length(x = erase.final) %>% (function(x) as.numeric(x)/1000) # as.numeric(.) %>% "/" (1000) # convert from meter to km!
  erase.final$L_trans <- ifelse(erase.final$Type == 2, erase.final$L, trans(x = erase.final$L, trans.k = trans.k))
  
  
  ## summarizing final data
  if(!quiet) cat("... get statistics: urban sprawl \n")
  
  if((!is.null(geom.boundary) & !force.extent))
  {
    df.result <- erase.final %>% sf::st_set_geometry(x = ., value = NULL) %>%
      data.table::as.data.table(.) %>%
      .[,list(L = sum(L, na.rm = TRUE),
              L_trans = sum(L_trans, na.rm = TRUE)), by = ID_BNDS] %>%
      dplyr::mutate(.data = ., FFE = L_trans/L*100)
  } else {
    df.result <- erase.final %>% sf::st_set_geometry(x = ., value = NULL) %>%
      data.table::as.data.table(.) %>%
      .[,list(L = sum(L, na.rm = TRUE),
              L_trans = sum(L_trans, na.rm = TRUE)),] %>%
      dplyr::mutate(.data = ., FFE = L_trans/L*100)
  }
  
  
  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of urban_sprawl: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")
  
  if(return.geom)
  {
    return(list(FFE = df.result, geom = erase.final, fishnet = fishnet))
  } else {
    return(df.result)
  }
  
} # end of function st_urban_sprawl