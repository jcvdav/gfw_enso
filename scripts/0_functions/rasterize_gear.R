rasterize_gear <- function(x, base_raster, gear = "trawlers", n_cores) {
  # Check to see if the file already exists
  file <- paste(gear, "tif", sep = ".")
  if(!file.exists(here("data", file))){
    
    # Set up parallel processing
    plan(multiprocess, workers = n_cores)
    
    # Rasterize at the year-month level
    gridded_effort_gear <- x %>% 
      filter(best_vessel_class == gear) %>% 
      select(year, month, x = lon_bin_center, y = lat_bin_center, kilowats) %>% 
      group_by(year, month, x, y) %>% 
      summarize(kilowats = sum(kilowats, na.rm = T)) %>% 
      ungroup() %>% 
      group_by(year, month) %>% 
      nest() %>%
      mutate(r = future_map(data, my_rasterize,
                            proj = proj_lonlat,
                            base_raster = base_raster),
             name = paste(year, month, sep = "-"))
    
    # Close connections
    plan(sequential)
    
    print("Created rasters") 
    
    # Create a stack and export it
    rasters <- stack(gridded_effort_gear$r)
    
    # Save the files
    writeRaster(x = rasters,
                filename = here("data", file))
    
    print("Saved rasters")
    
    # Remove temp files from disk
    rm_raster_tmp()
  }
}
