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
  
  # Add equal weight if no weight
  if(!"w" %in% names(origin)){
    origin$w = 1
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
    
    # Convert to proper matrix
    distM_y = matrix(as.numeric(distM_y), ncol = NCOL(distM_y))
    
    # Convert to km
    distM_y = distM_y/1000
    
    # Add one to avoid exploding
    distM_y = distM_y + 1
    
    # Inverted distance (inverted by theta)
    distM_y = distM_y^(-theta)
    
    # Compute MA
    MA_y = apply(distM_y, 2, function(x){
      sum(x*origin_y$w) # Multiplied by weight
    })
    
    # Res
    res_y = data.frame(
      Year = y,
      GIS_ID = destination_points$GIS_ID,
      MA = MA_y
    )
    
    if(verbose){
      cat("\nEstimated MA for",y)
    }
    
    return(res_y)
  }
  
  return(res)
}
