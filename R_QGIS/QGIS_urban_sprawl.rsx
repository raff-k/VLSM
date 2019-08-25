##VLSM=group
##Urban Sprawl=name
##Tool=selection "sf";"grass";"saga"
##Urban=vector
##Boundary=vector
##set_Boundary_to_NULL=boolean FALSE
##Mesh_Size_X=number 100
##Mesh_Size_Y=number 100
##Extent=string "c(xmin, xmax, ymax, ymin)"
##quiet=boolean FALSE
##do_preProcessing=boolean TRUE
##Environment_SAGA=string "C:/OSGeo4W64/apps/saga-6.3.0"
##Environment_GRASS=string "c('C:/OSGeo4W64/','grass76','osgeo4W')"
##Output_FFE=output table
##Output_Fishnet=output vector


# LOAD REQUIRED LIBRARIES
if(!require("pacman")) install.packages("pacman")
if(!require("devtools")) install.packages("devtools")
if(!require("VLSM")) devtools::install_github("raff-k/VLSM")

pacman::p_load(VLSM, raster, sf, sp, dplyr, RSAGA, rgrass7, link2GI)

# check input
if(set_Boundary_to_NULL){
  sf.Boundary <- NULL
  extent <- eval(parse(text = Extent))
  
  if(class(extent) != "numeric"){ stop("extent must be numeric!")}
  
} else { 
  # conversion to sf
  sf.Boundary <- Boundary %>% sf::st_as_sf()
  extent <- NULL
}


tool <- c("sf", "grass", "saga")[Tool+1]

if(tool == "grass"){
  # ... define a raster for a GRASS GIS region using
  # the administrative boundaries as bounding box
  r.GRASS <- raster::extent(sf.Boundary) %>%
             raster::raster(., crs = raster::crs(sf.Boundary))
  
  # ... initialisation of GRASS GIS "on the fly"
  GRASS_INIT <- link2GI::linkGRASS7(x = r.GRASS, default_GRASS7 = eval(parse(text = Environment_GRASS)))
}


if(tool == "saga"){
  env.rsaga <- RSAGA::rsaga.env(path = Environment_SAGA)
} else {
  env.rsaga <- NULL
}

# conversion to sf
sf.Urban <- Urban %>% sf::st_as_sf()
crs.Urban <- sf.Urban %>% sf::st_crs(x = .)


cat("\n ############################# \n")
cat("START st_urban_sprawl() \n")
cat("############################# \n \n")

result.FFE <- st_urban_sprawl(tool = tool, 
                              geom.urban = sf.Urban, 
                              dist = c(Mesh_Size_X, Mesh_Size_Y), 
                              env.rsaga = env.rsaga, 
                              geom.boundary = sf.Boundary,
                              extent = extent,
                              return.geom = TRUE,
                              quiet = quiet, 
                              do.preProcessing = do_preProcessing)

cat("\n ############################# \n")
cat("FINISHED st_urban_sprawl() \n")
cat("############################# \n")


Output_FFE<-result.FFE$FFE

result.FFE$geom <- result.FFE$geom %>% sf::st_set_crs(x = , value = crs.Urban)
output.geom <- result.FFE$geom %>% sf::as_Spatial(.)
Output_Fishnet<-output.geom
