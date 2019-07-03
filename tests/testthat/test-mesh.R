context("st_mesh")
pacman::p_load(sf, data.table, dplyr)

test_that("st_mesh() yields correct output", {
  nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  
  expect_output(st_mesh(geom.frag = nc[1:20,], total.area = 12.626/10000))
  
  expect_is(
    st_mesh(geom.frag = nc[1:20,], total.area = 12.626/10000),
    "data.frame"
  )
})
