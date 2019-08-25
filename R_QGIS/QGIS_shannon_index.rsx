##VLSM=group
##Shannon Index=name
##Tool=selection "sf";"grass"
##Input=vector
##Field=string
##Boundary=vector
##set_Boundary_to_NULL=boolean FALSE
##quiet=boolean FALSE
##do_preProcessing=boolean TRUE
##Environment_GRASS=string "c('C:/OSGeo4W64/','grass76','osgeo4W')"
##Output_SHDI=Output table



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
}


tool <- c("sf", "grass")[Tool+1]

if(tool == "grass"){
  # ... define a raster for a GRASS GIS region using
  # the administrative boundaries as bounding box
  r.GRASS <- raster::extent(sf.Boundary) %>%
             raster::raster(., crs = raster::crs(sf.Boundary))
  
  # ... initialisation of GRASS GIS "on the fly"
  GRASS_INIT <- link2GI::linkGRASS7(x = r.GRASS, default_GRASS7 = eval(parse(text = Environment_GRASS)))
}


# conversion to sf
sf.Input <- Input %>% sf::st_as_sf()



cat("\n ############################# \n")
cat("START st_shannon_index() \n")
cat("############################# \n \n")

result.SHDI <- st_shannon_index(tool = tool, 
                                x = sf.Input,
                                field = Field,
                                geom.boundary = sf.Boundary,
                                return.geom = FALSE,
                                quiet = quiet, 
                                do.preProcessing = do_preProcessing)

cat("\n ############################# \n")
cat("FINISHED st_shannon_index() \n")
cat("############################# \n")


Output_SHDI<-result.SHDI$SHDI

