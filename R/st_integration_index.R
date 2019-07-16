#' Calculate integration index
#'
#' This function calculates an index defining the intergration of new land use area into conisting land use area
#' of the same land use class conisdering two different time steps.
#'
#' @param tool tool to perform erase-vector-operation. Default is \code{"sf"}. However, for complex polygons \code{"grass"} is highly recommended. Fot the use of \code{"grass"} a valid GRASS GIS-session mus be initiated. \code{"saga"} is also supported, then \code{env.rsaga} must be properly set
#' @param geom.old object of class \code{sf} representing a land use class of a previous time step
#' @param geom.new object of class \code{sf} representing a land use class of a following time step
#' @param geom.boundary polygon of class \code{sf} representing subregions, e.g. administrative boundaries
#' @param tol tolerance value for overlapping area meter square
#' @param precision precision for process using \code{sf::st_set_precision}. See \link[sf]{st_precision}. Default: \code{0}
#' @param env.rsaga environment of \code{SAGA GIS}. If st_erase fails then \code{SAGA GIS} erase is used. Default: \code{NULL}, but in function call if not set: \link[RSAGA]{rsaga.env}
#' @param snap.rgrass similar to \code{sf::st_set_precision}. Snapping threshold for boundaries. Default: \code{1e-04}
#' @param dist.new buffer distance in mapping units around new geometry (result of erase) using \code{sf::st_buffer}. Default: \code{0} 
#' @param override If \code{TRUE} projection of geometry-operation is replaced by projection of \code{geom.old}. This flag prevents errors when the original datum is not found by the \code{tool}. Dafault: \code{FALSE} 
#' @param return.geom If set to \code{TRUE}, intermediate geometries are returned as well. Default: \code{FALSE}
#' @param quiet show output on console. Default: \code{FALSE}
#' @note If the unique border line has incomprehensible gabs(can happen for "complicated" geometries), it can help to use a small buffer distance (< 0.05, e.g. 1e-04) by \code{dist.new} to get a complete edge line
#' Code is based on the following references:
#' \itemize{
#'   \item Meinel, G., & Winkler, M. (2002). Spatial analysis of settlement and open land trends in urban areas on basis of RS data œ studies of five European cities over a 50-year period.
#'   \item Siedentop, S., Heiland, S., Lehmann, I., & Schauerte-Lüke, N. (2007). Regionale Schlüsselindikatoren nachhaltiger Flächennutzung für die Fortschrittsberichte der Nationalen Nachhaltigkeitsstrategie–Flächenziele (Nachhaltigkeitsbarometer Fläche). Abschlussbericht, BBR-Forschungen, H, 130.
#' }
#' Depending on the selected \code{tool}, the result of the vector-operation can differ significantly!
#' @return
#' Vector with integration index (completly integrated: 2/3 < R < 1; good integrated: 1/3 < R < 2/3; low integrated: 0 < R < 1/3; not integrated: 0)
#'
#'
#' @keywords simple feature, shape index, interior edge ratio
#'
#'
#' @export
#'
st_integration_index = function(tool = "sf", geom.old, geom.new, geom.boundary = NULL, tol = 0.1, precision = 0,
                                env.rsaga = NULL, snap.rgrass = 1e-5, dist.new = 0, override = TRUE, return.geom = FALSE, quiet = FALSE){
  
  # get start time of process
  process.time.start <- proc.time()

  ## check input
  if(missing(geom.old) || missing(geom.new)){ stop('Input is missing!')}
  
  ## check and set precision
  if(!quiet) cat("... check and set st_precision \n")
  if(sf::st_precision(geom.old) != precision)
  {
    geom.old <- geom.old %>% sf::st_set_precision(x = ., precision = precision) %>%
      lwgeom::st_make_valid(.) %>%
      sf::st_collection_extract(x = ., type = "POLYGON")
  }
  
  if(sf::st_precision(geom.new) != precision)
  {
    geom.new <- geom.new %>% sf::st_set_precision(x = ., precision = precision) %>%
      lwgeom::st_make_valid(.) %>%
      sf::st_collection_extract(x = ., type = "POLYGON")
  }
  
  ## check validity of geometries
  if(!all(sf::st_is_valid(geom.old))){ stop('Input of "geom.old" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  if(!all(sf::st_is_valid(geom.new))){ stop('Input of "geom.new" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  if(!is.null(geom.boundary) && !all(sf::st_is_valid(geom.boundary))){ stop('Input of "geom.boundary" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  
 
  
  # # # # START CALCULATION OF INTEGRATION INDEX
  ## check for overlapping polygon
  
  if(tool == "saga" | tool == "sf")
  {
    ## common area
    if(!quiet) cat("... intersection of input geometries \n")
    inter <- suppressWarnings(sf::st_intersection(x = geom.old, y = geom.new) %>%
                                sf::st_collection_extract(x = ., type = c("POLYGON")))#  %>%
    
    ## new area
    if(!quiet) cat('... erase intersection from "geom.new" (this can take a while!) \n')
    
    if(tool == "saga")
    {
      if(is.null(env.rsaga))
      {
        env.rsaga <-  RSAGA::rsaga.env()
      }
      
      erase <- rsaga_erase(x = geom.new, y = inter, method = "1",
                           split = "1", env.rsaga = env.rsaga) %>%
        .[which(x = as.numeric(sf::st_area(.)) >= tol),]
      
    } else {
      erase <- tryCatch({
        suppressWarnings(st_erase(x = geom.new, y = inter, precision = precision) %>%
                           sf::st_collection_extract(x = ., type = c("POLYGON")) %>%
                           sf::st_cast(x = ., to = "POLYGON") %>%
                           .[which(x = as.numeric(sf::st_area(.)) >= tol),])
      }, error = function(e){
        warning(paste('SAGA GIS is used due to error in st_erase():', e))
        if(is.null(env.rsaga))
        {
          env.rsaga <-  RSAGA::rsaga.env()
        }
        tmp.erase <- rsaga_erase(x = geom.new, y = inter, method = "1",
                                 split = "1", env.rsaga = env.rsaga) %>%
          .[which(x = as.numeric(sf::st_area(.)) >= tol),]
        return(tmp.erase)
      })
    } # end of use.saga
  } else if(tool == "grass"){
    if(!quiet) cat("... intersection of input geometries \n")
    inter <-  rgrass_overlay(x = geom.old, y = geom.new, operator = "and", snap = snap.rgrass) %>%  # and: also known as 'intersection' in GIS
                  dplyr::select("geometry") %>%
                  dplyr::mutate(ID_inter = 1:nrow(.))
    
    if(!quiet) cat('... erase intersection from "geom.new" (this can take a while!) \n')
    erase <- rgrass_overlay(x = geom.new, y = inter, operator = "not", snap = snap.rgrass) %>% # not: also known as 'difference' 
                    .[which(x = as.numeric(sf::st_area(.)) >= tol),]
  } else {
    stop("No valid input tool! \n")
  }
  
  if(override)
  {
    if(!quiet) cat("... override projection \n")
    erase <- suppressWarnings(erase %>% sf::st_set_crs(., value = geom.old %>% sf::st_crs(.)))
    inter <- suppressWarnings(inter %>% sf::st_set_crs(., value = geom.old %>% sf::st_crs(.)))
  }
  
  if(!quiet) cat('... post-process erase geometry \n')
  erase <- erase %>% preProcessing(x = ., split = TRUE)
  
  
  if(!quiet) cat('... conversion to lines \n')
  if(dist.new > 0)
  {
    # ... it is basically not a line!
    line.erase <-  sf::st_buffer(x = erase, dist = dist.new)  %>% sf::st_set_precision(x = ., precision = precision) # preProcessing(x = ., split = TRUE) %>% dplyr::rename("ID_x" = "ID")
  } else {
    line.erase <-  sf::st_cast(x = erase, to = "MULTILINESTRING") %>% sf::st_set_precision(x = ., precision = precision)
  }
  line.inter <- sf::st_cast(x = inter, to = "MULTILINESTRING") %>% sf::st_set_precision(x = ., precision = precision)
  
  ## common border
  if(!quiet) cat('... find border lines by intersection \n')
  unique.border <- suppressWarnings(sf::st_intersection(x = line.inter, y = line.erase) %>%
                                      sf::st_collection_extract(x = ., type = c("LINESTRING")))
  
  ## check boundary constraints
  if(!is.null(geom.boundary))
  {
    geom.boundary$ID_BOUNDS <- 1:nrow(geom.boundary)
    geom.boundary <- geom.boundary[, c("ID_BOUNDS", "geometry")]
    
    if(!quiet) cat('... intersection with boundaries \n')
    unique.border <- suppressWarnings(sf::st_intersection(x = geom.boundary, y = unique.border) %>%
                                        sf::st_collection_extract(x = ., type = c("LINESTRING")) %>%
                                        sf::st_cast(., "MULTILINESTRING") %>% sf::st_cast(., "LINESTRING")) # cast is necessairy to split multi-object
    
    dt.unique.border <- unique.border %>%
      sf::st_set_geometry(x = ., value = NULL) %>%
      data.table::as.data.table(.)
    
    
    erase <- suppressWarnings(sf::st_intersection(x = geom.boundary, y = erase) %>%
                                sf::st_collection_extract(x = ., type = c("POLYGON")) %>%
                                sf::st_cast(., "MULTIPOLYGON") %>% sf::st_cast(., "POLYGON"))
    dt.erase  <- erase  %>%
      sf::st_set_geometry(x = ., value = NULL) %>%
      data.table::as.data.table(.)
    
    
    if(!quiet) cat('... get statistics and calculate index \n')
    dt.unique.border$L <- sf::st_length(x = unique.border) %>% as.numeric() # units::drop_units(x = .)
    dt.erase$P <- st_perimeter(x = erase)
    
    dt.result.UB <- dt.unique.border[,list(L = sum(L, na.rm = TRUE)), by = ID_BOUNDS]
    dt.result.E <- dt.erase[,list(P = sum(P, na.rm = TRUE)), by = ID_BOUNDS]
    
    dt.result <- merge(x = dt.result.E, y = dt.result.UB, by = "ID_BOUNDS") %>%
      dplyr::mutate(.data = ., R = L/P)
    
  } else {
    if(!quiet) cat('... get statistics and calculate index \n')
    
    dt.result <- data.table::data.table(P = sum(st_perimeter(x = erase), na.rm = TRUE),
                                        L = sum((sf::st_length(x = unique.border) %>% as.numeric(.)), na.rm = TRUE)) %>% # units::drop_units(x = .))) %>%
      dplyr::mutate(.data = ., R = L/P)
    
  } # end of if-else boundary check
  
  
  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of st_integration_index: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")
  
  if(return.geom)
  {
    return(list(integration_index = dt.result, unique_border = unique.border, new_area = erase, inter_area = inter))
  } else {
    return(dt.result)
  }
} # end of function st_integration_index