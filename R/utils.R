# Function to create ID - Description tibble
make_dict <- function(data, id, desc){
  id <- enquo(id)
  desc <- enquo(desc)
  
  data %>% 
    group_by(!!id, !!desc) %>% 
    summarise() %>% 
    arrange(!!desc)
}

# Function to compute distances between consecutive rows
compute_distances <- function(data){

  rows <- nrow(data) - 1
  
  distances <- vector("list", rows)

  for (i in 1:(rows)){
    lon_from <- data$LON[[i]]
    lat_from <- data$LAT[[i]]

    lon_to <- data$LON[[i + 1]]
    lat_to <- data$LAT[[i + 1]]

    distances[[i]] <- distm(
      c(lon_from, lat_from), 
      c(lon_to, lat_to),
      fun = distHaversine
    )
  }
  
  distances <- distances %>% unlist
  
}



