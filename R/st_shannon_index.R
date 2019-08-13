#' Calculate the Shannon's Diversity Index 
#'
#' This function calculates the Shannon's Diversity Index for landscapes.
#'
#' @param tool tool to perform dissolve operation. Default is \code{"sf"}. However, for complex polygons \code{"grass"} is highly recommended. Fot the use of \code{"grass"} a valid GRASS GIS-session mus be initiated. \code{"saga"} is NOT supported
#' @param x polygon of class \code{sf}
#' @param field field name or table column (\code{character}) on which the shannon index is calculated
#' @param do.preProcessing If \code{TRUE} (default), the input of \code{geom.frag} is, first, dissolved by field, and second, splitted to multi-parts. By this step it is assured, that polygon connected to each other are summarized
#' @param return.geom If set to \code{TRUE}, geometries (e.g. dissolved) are returned as well. Default: \code{FALSE}
#' @param quiet If set to \code{FALSE} actual state is printed to console. Default: \code{TRUE}.
#' @note Code is based on the following references:
#' \itemize{
#'   \item McGarigal, K., & Marks, B. J. (1995). FRAGSTATS: spatial pattern analysis program for quantifying landscape structure. Gen. Tech. Rep. PNW-GTR-351. Portland, OR: US Department of Agriculture, Forest Service, Pacific Northwest Research Station. 122 p, 351.
#' }
#' @return
#' \code{list} with overall Shannon's Diversity Index, and index for each landscape classes. If \code{return.geom} is \code{TRUE} than \code{list} with result and geometry on which SHDI is based is returned.
#
#'
#' @keywords Shannon's Diversity Index 
#'
#'
#' @export
#'
st_shannon_index = function(tool = "sf", x, field, do.preProcessing = TRUE, return.geom = FALSE, quiet = TRUE)
{
  # get start time of process
  process.time.start <- proc.time()
  
  ## check input
  if(missing(x)){ stop('Input of "x" is missing!')}
  if(!missing(x) && !("sf" %in% class(x))){ stop('Input of "x" is not of class "sf"!')}
  if(missing(field)){ stop('Input of "field" is missing!')}

  
  ## check validity of geometries
  if(!all(sf::st_is_valid(x))){ stop('Input of "x" contains not valid geometries. Please try lwgeom::st_make_valid().')}


  ## subset columns of data.frame
  x <- x %>% dplyr::select(c(field, "geometry"))

  if(do.preProcessing)
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
  
  if(!quiet) cat("... calculate patch areas and summarize data \n")
  ## add Area in m_msq 
  x$A_m_sq <- x %>% sf::st_area(.) %>% as.numeric(x)

  ## get total data
  x.summerize <- x %>% sf::st_drop_geometry(.) %>% 
                       dplyr::group_by(!!as.name(field)) %>% 
                       dplyr::summarise(A_T_m_sq = sum(A_m_sq, na.rm = TRUE), n = n()) %>%
                       dplyr::mutate(A_TT_m_sq = sum(A_T_m_sq, na.rm = TRUE),
                                     P = A_T_m_sq/A_TT_m_sq,
                                     P_LogP = P * log(P))
  
  ## get SHDI
  SHDI <- sum(x.summerize$P_LogP, na.rm = TRUE)*(-1)
  SHDI_norm <- sum(x.summerize$P_LogP, na.rm = TRUE)*(-1)/log(nrow(x.summerize))
  
  if(!quiet) cat("... merge back summarized data and calculate SHDI for internal diversity \n")
  ## merge data back
  x.out <- x %>% merge(x = ., y = x.summerize %>% dplyr::select(-c("A_TT_m_sq", "P", "P_LogP")),
                       all.x = TRUE, by = field)


  ## calculate P (proportion of the landscape occupied by patch type (class) i)
  x.out <- x.out %>% dplyr::mutate(P = A_m_sq/A_T_m_sq,
                                   P_LogP = P * log(P))


  ## summarize based on classes (internal diversity)
  SHDI.internal.table <- x.out %>% sf::st_drop_geometry(.) %>%
                          dplyr::group_by(!!as.name(field)) %>%
                          dplyr::summarise(SHDI_intern = sum(P_LogP, na.rm = TRUE)*(-1), n = unique(n)) %>%
                          dplyr::mutate(SHDI_intern_norm = SHDI_intern/log(n)) %>%
                          data.table::as.data.table(.)
                          

  process.time.run <- proc.time() - process.time.start
  if(quiet == FALSE) cat("------ Run of st_shannon_index: " , round(process.time.run["elapsed"][[1]]/60, digits = 4), " Minutes \n")
  
  if(return.geom)
  {
    return(list(SHDI = SHDI, SHDI_table = x.summerize, SHDI_internal_table = SHDI.internal.table, geom = x.out))
  } else {
    return(list(SHDI = SHDI, SHDI_table = x.summerize, SHDI_internal_table = SHDI.internal.table))
  }
} # end of function st_shannon_index
