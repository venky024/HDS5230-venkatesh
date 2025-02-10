library(tidyverse)
library(microbenchmark)

data <- read_excel("clinics.xls")

data <- data %>%
  mutate(
    locLat = as.numeric(locLat),
    locLong = as.numeric(locLong)
  )

haversine <- function(lat1, lon1, lat2, lon2) {
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  r <- 3959
  
  return(r * c)
}

# Reference point (first clinic)
ref_lat <- data$locLat[1]
ref_long <- data$locLong[1]

# Method 1: For loop
method_loop <- function() {
  distances <- numeric(nrow(data))
  for(i in 1:nrow(data)) {
    distances[i] <- haversine(ref_lat, ref_long,
                              data$locLat[i],
                              data$locLong[i])
  }
  return(distances)
}

# Method 2: Using apply
method_apply <- function() {
  apply(data, 1, function(row) {
    haversine(ref_lat, ref_long,
              as.numeric(row['locLat']),
              as.numeric(row['locLong']))
  })
}

# Method 3: Vectorized
method_vectorized <- function() {
  haversine(ref_lat, ref_long,
            data$locLat,
            data$locLong)
}

cat("Running benchmarks...\n")
results <- microbenchmark(
  loop = method_loop(),
  apply = method_apply(),
  vectorized = method_vectorized(),
  times = 100
)

print(results)
