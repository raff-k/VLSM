##VLSM=group
##Mesh=name
##Fragmentation=vector
##Boundary=vector
##set_Boundary_to_NULL=boolean FALSE
##Total_Area=number -99999
##conv=number 10000
##quiet=boolean FALSE
##do_preProcessing=boolean TRUE
##Output_MESH=Output table

# LOAD REQUIRED LIBRARIES
if(!require("pacman")) install.packages("pacman")
if(!require("devtools")) install.packages("devtools")
if(!require("VLSM")) devtools::install_github("raff-k/VLSM")

pacman::p_load(VLSM, sf, sp, dplyr)

# check input
if(Total_Area == -99999){total.area <- NULL} else { total.area <- Total_Area }
if(set_Boundary_to_NULL){
  sf.Boundary <- NULL
} else { 
  # conversion to sf
  sf.Boundary <- Boundary %>% sf::st_as_sf()
}

# conversion to sf
sf.Fragmentation <- Fragmentation %>% sf::st_as_sf()



cat("\n ############################# \n")
cat("START st_mesh() \n")
cat("############################# \n \n")

result.mesh <- st_mesh(geom.frag = sf.Fragmentation, 
        geom.boundary = sf.Boundary, 
        total.area = total.area, 
        conv = conv, 
        return.geom = FALSE,
        quiet = quiet, 
        do.preProcessing = do_preProcessing)
        
cat("\n ############################# \n")
cat("FINISHED st_mesh() \n")
cat("############################# \n")


Output_MESH<-result.mesh$mesh