# Function to rasterize each month of fishing effort
my_rasterize <- function(x, res = 0.1, proj, base_raster) {
  # Create raster
  r <- rasterFromXYZ(x,
                     res = res,
                     crs = proj)
  
  # Adjust extent
  bb <- extent(-180, 180, -90, 90)
  r2 <- extend(r, bb)
  r3 <- crop(r2, as(bb, "SpatialPolygons"))
  r4 <- projectRaster(r3,
                      base_raster,
                      crs = proj_beh,
                      method = "ngb",
                      over = T)
  
  return(r4)
}
