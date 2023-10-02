# Functions
#
# Date updated:   2023-09-15
# Auhtor:         Christian Vedel 
# Purpose:        Functions used in the rest of the project


# ==== pretty_pct(x, digits = 4) ====
# Makes pretty pct string
pretty_pct = function(x, digits = 4){
  x = signif(x, digits = digits)
  x = x*100
  x = paste(x,"%",sep ="")
  return(x)
}

# ==== MA(destination, origin) ====
# Computes the Market Acces at 'destination' of the objects in origin 
# If 'Year' appears in 'origin' it will return a panel

# destination:  st object with destinations where MP should be estimated 
# origin:       The objects to calculate MA of. Should contain long and lat.
# theta:        Distance elasticity (default: theta = 1)
# verbose:      Should updates be printed?

MA = function(destination, origin, theta = 1, verbose = FALSE){
  require(foreach)
    # destination = shape_parishes
    # origin = assembly_houses
  
  # Extract points
  destination_points = destination %>% 
    as.data.frame() %>% 
    select(GIS_ID, long, lat) %>% 
    st_as_sf(
      ., coords = c("long", "lat"),
      crs = st_crs(destination)
    )
  
  # Add fake year if no year
  if(!"Year" %in% names(origin)){
    origin$Year = 9999
  }
  
  # Remove missing
  origin = origin %>% drop_na(lat, long)
  
  # Convert origin to spatial object
  origin_sp = st_as_sf(
    origin, coords = c("long", "lat"),
    crs = st_crs(destination)
  )
  
  # Compute distance in loop
  res = foreach(y = sort(unique(origin_sp$Year)), .combine = "bind_rows") %do% {
    origin_y = origin_sp %>% 
      filter(Year == y)
    
    # Compute distance matrix
    distM_y = st_distance(origin_y, destination_points)
    
    # Inverted distance (inverted by theta)
    distM_y = distM_y^(-theta)
    
    # Compute MA
    MA_y = apply(distM_y, 2, sum) 
    
    # Res
    res_y = data.frame(
      Year = y,
      GIS_ID = destination_points$GIS_ID,
      MA = MA_y
    )
    
    if(verbose){
      cat("\nEstiamted MA for",y)
    }
    
    return(res_y)
  }
  
  return(res_y)
}
