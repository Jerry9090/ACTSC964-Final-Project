library(ncdf4)
library(tidyverse)

# 1. Read the NetCDF file
nc_file <- "C:/Users/z2262zha/Downloads/cru_ts4.09.1901.2024.tmx.dat.nc/cru_ts4.09.1901.2024.tmx.dat.nc"
nc <- nc_open(nc_file)

# 2. Get dimensions and variables
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
time <- ncvar_get(nc, "time")
tmx <- ncvar_get(nc, "tmx")  # lon x lat x time

# 3. Find the nearest longitude and latitude index
get_nearest_index <- function(coord, grid) {
  which.min(abs(grid - coord))
}

# Coordinates for Phoenix: 112.07W, 33.45N
phoenix_tmx <- c(
  lon = get_nearest_index(-112.07, lon),
  lat = get_nearest_index(33.45, lat)
)

# 4. Convert the time axis to year and month
origin <- as.Date("1900-01-01")
dates <- origin + time
years <- as.numeric(format(dates, "%Y"))
months <- as.numeric(format(dates, "%m"))

# 5. Function to extract annual maximum of monthly mean temperature for a given location
get_yearly_max <- function(i_lon, i_lat) {
  ts <- tmx[i_lon, i_lat, ]
  df <- data.frame(date = dates, year = years, month = months, tmx = ts)
  df %>%
    group_by(year) %>%
    summarize(yearly_max_tmx = max(tmx, na.rm = TRUE)) %>%
    ungroup()
}

# 6. Extract data frame for the selected city
phoenix_tmx <- get_yearly_max(phoenix_tmx["lon"], phoenix_tmx["lat"])

# Phoenix: Save as "Phoenix_tmx.txt"
write.table(phoenix_tmx$yearly_max_tmx,
            file = "C:/Users/z2262zha/Downloads/cru_ts4.09.1901.2024.tmx.dat.nc/Phoenix_tmx.txt",
            row.names = FALSE,
            col.names = FALSE,
            sep = "\t",
            quote = FALSE)
