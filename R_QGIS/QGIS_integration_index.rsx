##VLSM=group
##Integration Index=name
##Tool=selection "sf";"grass";"saga"
##Input_Old=vector
##Input_New=vector
##Boundary=vector
##set_Boundary_to_NULL=boolean FALSE
##Tolerance=number 0.1
##Snap_GRASS=number 0.0001
##Buffer_Distance_new=number 0
##quiet=boolean FALSE
##Environment_SAGA=string "C:/OSGeo4W64/apps/saga-6.3.0"
##Environment_GRASS=string "c('C:/OSGeo4W64/','grass76','osgeo4W')"
##Output_Integration_Index=output table
##Output_Unique_Boundary=output vector



# LOAD REQUIRED LIBRARIES
if(!require("pacman")) install.packages("pacman")
if(!require("devtools")) install.packages("devtools")
if(!require("VLSM")) devtools::install_github("raff-k/VLSM")

pacman::p_load(VLSM, raster, sf, sp, dplyr, RSAGA, rgrass7, link2GI)

# check input
if(set_Boundary_to_NULL){
  sf.Boundary <- NULL
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
sf.Old <- Input_Old %>% sf::st_as_sf()
sf.New <- Input_New %>% sf::st_as_sf()


cat("\n ############################# \n")
cat("START st_integration_index() \n")
cat("############################# \n \n")

result.Integration_Index <- st_integration_index(tool = tool, 
                                                 geom.old = sf.Old, 
                                                 geom.new = sf.New,
                                                 geom.boundary = sf.Boundary,
                                                 tol = Tolerance, 
                                                 env.rsaga = env.rsaga,
                                                 snap.rgrass = Snap_GRASS,
                                                 dist.new = Buffer_Distance_new,
                                                 return.geom = TRUE,
                                                 quiet = quiet)

cat("\n ############################# \n")
cat("FINISHED st_integration_index() \n")
cat("############################# \n")


Output_Integration_Index<-result.Integration_Index$integration_index

output.geom <- result.Integration_Index$unique_border %>% sf::as_Spatial(.)
Output_Unique_Boundary<-output.geom
