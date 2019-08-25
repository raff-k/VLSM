##VLSM=group
##Edge Length=name
##Input_x=vector
##Input_y=vector
##Buffer_Distance_x=number 0
##Buffer_Distance_y=number 0
##Boundary=vector
##set_Boundary_to_NULL=boolean FALSE
##do_preProcessing=boolean TRUE
##quiet=boolean FALSE
##Output_EdgeLength=output table
##Output_EdgeLine=output vector



# LOAD REQUIRED LIBRARIES
if(!require("pacman")) install.packages("pacman")
if(!require("devtools")) install.packages("devtools")
if(!require("VLSM")) devtools::install_github("raff-k/VLSM")

pacman::p_load(VLSM, sf, sp, dplyr)

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


# conversion to sf
sf.x <- Input_x %>% sf::st_as_sf()
sf.y <- Input_y %>% sf::st_as_sf()


cat("\n ############################# \n")
cat("START st_edge_length() \n")
cat("############################# \n \n")

result.EdgeLength <- st_edge_length(x = sf.x, 
                              y = sf.y, 
                              dist.x = Buffer_Distance_x, 
                              dist.y = Buffer_Distance_y,
                              geom.boundary = sf.Boundary,
                              return.geom = TRUE,
                              quiet = quiet, 
                              do.preProcessing = do_preProcessing)

cat("\n ############################# \n")
cat("FINISHED st_edge_length() \n")
cat("############################# \n")


Output_EdgeLength<-result.EdgeLength$edge_length

output.geom <- result.EdgeLength$geom_edge %>% sf::as_Spatial(.)
Output_EdgeLine<-output.geom
