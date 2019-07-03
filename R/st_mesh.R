#' Calculate the effective mesh size
#'
#' This function calculates the effective mesh size.
#'
#' @param geom.frag polygon of class \code{sf} representing the fragmentation geometry
#' @param geom.boundary polygon of class \code{sf} representing subregions, e.g. administrative boundaries
#' @param total.area Numeric value representing size for area. Only to use if \code{geom.boundary} is not present. Value must match with the conversion constant \code{c} (default hectare). Default: \code{NULL}
#' @param conv constant to convert original square meter output. Default: \code{10000} to convert to hectare. If set to \code{1}, than meter square is the result.
#' @param do.preProcessing If \code{TRUE} (default), the input of \code{geom.frag} is, first, dissolved to single part feature, and second, splitted to multi-parts. By this step it is assured, that polygon connected to each other are summarized
#' @param return.geom If set to \code{TRUE}, intermediate geometries are returned as well. Default: \code{FALSE}
#' @param quiet If set to \code{FALSE} actual state is printed to console. Default: \code{TRUE}.
#' @import data.table
#' @note Code is based on the following references:
#' \itemize{
#'   \item Moser, B., Jaeger, J. A., Tappeiner, U., Tasser, E., & Eiselt, B. (2007). Modification of the effective mesh size for measuring landscape fragmentation to solve the boundary problem. Landscape ecology, 22(3), 447-459.
#'   \item Jaeger, J. A. (2000). Landscape division, splitting index, and effective mesh size: new measures of landscape fragmentation. Landscape ecology, 15(2), 115-130.
#' }
#' @return
#' If \code{return.geom} is \code{TRUE} than \code{list} with result and geometry is returned. Otherwhise result is returned in form of a \code{data.frame}
#
#'
#' @keywords simple feature, mesh, effective mesh size
#'
#'
#' @export
#'
st_mesh = function(geom.frag, geom.boundary = NULL, total.area = NULL, conv = 10000, do.preProcessing = TRUE, return.geom = FALSE, quiet = TRUE)
{
  # get start time of process
  process.time.start <- proc.time()
  
  ## check input
  if(missing(geom.frag)){ stop('Input of "geom.frag" is missing!')}
  if(!missing(geom.frag) && !("sf" %in% class(geom.frag))){ stop('Input of "geom.frag" is not of class "sf"!')}
  if(!is.null(geom.boundary) && !("sf" %in% class(geom.boundary))){ stop('Input of "geom.boundary" is not of class "sf"!')}
  if(is.null(geom.boundary) && (is.null(total.area) || !is.numeric(total.area))){
    stop('If input of "geom.boundary" is null, then input of "total.area" must be a numeric value representing the size of the study area.')
  }
  
  
  ## check validity of geometries
  if(!all(sf::st_is_valid(geom.frag))){ stop('Input of "geom.frag" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  if(!is.null(geom.boundary) && !all(sf::st_is_valid(geom.boundary))){ stop('Input of "geom.boundary" contains not valid geometries. Please try lwgeom::st_make_valid().')}
  
  
  if(do.preProcessing)
  {
    if(!quiet) cat("... union geometries to a single geometry with resolved boundaries \n")
    geom.frag <- geom.frag %>% sf::st_union(.)
    
    if(!quiet) cat("... split multi-parts to single-parts polygon \n")
    geom.frag <- geom.frag %>% sf::st_cast(., "POLYGON") %>% sf::st_sf(ID_FRAG = 1:length(.), geometry = .)
  } else {
    geom.frag$ID_FRAG <- 1:nrow(geom.frag) ## add unique IDs
  }
  
  if(!is.null(geom.boundary)){ geom.boundary$ID_BOUNDS <- 1:nrow(geom.boundary) } # ## add unique IDs
  
  ## add Area in m_msq / conversion factor
  geom.frag$A_FRAG <- sf::st_area(geom.frag) %>% (function(x = ., conv_fac = conv) as.numeric(x)/conv_fac)
  if(!is.null(geom.boundary)){ geom.boundary$A_BOUNDS <- sf::st_area(geom.boundary) %>% (function(x, conv = conv) as.numeric(x)/conv)}
  
  
  ## subset columns of data
  geom.frag <- geom.frag[, c("ID_FRAG", "A_FRAG", "geometry")]
  if(!is.null(geom.boundary)){geom.boundary <- geom.boundary[, c("ID_BOUNDS", "A_BOUNDS", "geometry")]}
  
  
  # # # CALCULATE MESH SIZE INDICES
  ## start calculation
  if(!is.null(geom.boundary))
  {
    ## get intersection
    if(!quiet) cat("... intersection to boundary \n")
    inter <- suppressWarnings(sf::st_intersection(x = geom.boundary, y = geom.frag))
    inter <- suppressWarnings(inter %>% sf::st_cast(., "MULTIPOLYGON") %>% sf::st_cast(., "POLYGON")) # cast is necessairy to split multi-polygons
    inter$A_FRAG_INTER <- sf::st_area(inter) %>% (function(x, conv = conv) as.numeric(x)/conv) # overwrite area of fragments
    
    ## calculation of mesh indices
    df.inter <- sf::st_set_geometry(x = inter, value = NULL) %>% data.table::as.data.table(.)
    
    df.inter.multi <-  df.inter[, list(A_BOUNDS = unique(A_BOUNDS), # splitted multi-parts are joined together
                                       A_FRAG = unique(A_FRAG),
                                       A_FRAG_INTER = sum(A_FRAG_INTER, na.rm = TRUE)), by = list(ID_BOUNDS, ID_FRAG)]
    
    if(!quiet) cat("... get statistics \n")
    ## cutting-out (CUT) procedure
    mesh.CUT <- df.inter[, list(Fg = unique(A_BOUNDS),
                                Fi = sum(A_FRAG_INTER^2, na.rm = TRUE),
                                Fi_count = length(!is.na(A_FRAG_INTER)),
                                Fi_sum = sum(A_FRAG_INTER, na.rm = TRUE),
                                Fi_min = min(A_FRAG_INTER, na.rm = TRUE),
                                Fi_max = max(A_FRAG_INTER, na.rm = TRUE),
                                Fi_ave = mean(A_FRAG_INTER, na.rm = TRUE),
                                Ci = sum((A_FRAG_INTER/A_BOUNDS)^2, na.rm = TRUE)), by = ID_BOUNDS] %>%
      dplyr::mutate(., Di = 1-Ci)  %>%
      dplyr::mutate(., Si = 1/Ci)  %>%
      dplyr::mutate(., mEff_CUT = Fi/Fg)
    
 
    ## cross-boundary connections (CBC) procedure
    mesh.CBC <- df.inter.multi[, list(Fg = unique(A_BOUNDS),
                                      FiErg_count = length(!is.na(A_FRAG)),
                                      FiErg_sum = sum(A_FRAG, na.rm = TRUE),
                                      FiErg_min = min(A_FRAG, na.rm = TRUE),
                                      FiErg_max = max(A_FRAG, na.rm = TRUE),
                                      FiErg_ave = mean(A_FRAG, na.rm = TRUE),
                                      CiErg = sum((A_FRAG/A_BOUNDS)^2, na.rm = TRUE),
                                      Fi_CBC1 = sum(A_FRAG_INTER*A_FRAG, na.rm = TRUE),
                                      Fi_CBC2 = sum(2*A_FRAG_INTER*A_FRAG-A_FRAG_INTER^2, na.rm = TRUE)), by = ID_BOUNDS] %>%
      dplyr::mutate(., DiErg = 1-CiErg)  %>%
      dplyr::mutate(., SiErg = 1/CiErg)  %>%
      dplyr::mutate(., mEff_CBC1 = Fi_CBC1/Fg) %>%
      dplyr::mutate(., mEff_CBC2 = Fi_CBC2/Fg)
    
   
    df.result <- merge(x = mesh.CUT, y = mesh.CBC[, -2], by = "ID_BOUNDS")
    
  } else {
    
    if(!quiet) cat("... get statistics \n") # ... based on total area
    df.result <- sf::st_set_geometry(x = geom.frag, value = NULL) %>% data.table::as.data.table(.)
    df.result <- df.result[, list(Fg = total.area,
                                  Fi = sum(A_FRAG^2, na.rm = TRUE),
                                  Fi_count = length(!is.na(A_FRAG)),
                                  Fi_sum = sum(A_FRAG, na.rm = TRUE),
                                  Fi_min = min(A_FRAG, na.rm = TRUE),
                                  Fi_max = max(A_FRAG, na.rm = TRUE),
                                  Fi_ave = mean(A_FRAG, na.rm = TRUE),
                                  Ci = sum((A_FRAG/total.area)^2, na.rm = TRUE)),] %>%
      dplyr::mutate(., Di = 1-Ci)  %>%
      dplyr::mutate(., Si = 1/Ci)  %>%
      dplyr::mutate(., mEff_CUT = Fi/total.area)
    
  }
  
  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of st_mesh: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")
  
  if(return.geom)
  {
    if(is.null(geom.boundary))
    {
      return(list(mesh = df.result, geom.frag = geom.frag))
    } else {
      return(list(mesh = df.result, geom.frag = geom.frag, geom.inter = inter))
    }
  } else {
    return(df.result)
  }
} # end of function st_mesh