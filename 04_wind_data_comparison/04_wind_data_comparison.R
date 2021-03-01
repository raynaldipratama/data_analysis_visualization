# LIBRARIES ----

# NetCDF in R
library(RNetCDF)

# Argos location filter
library(argosfilter)

# Manipulating and analysing data
library(tidyverse)

# Wind components transformation
library(rWind)

# Wind rose plot
library(openair)

# Taylor diagram
library(plotrix)

# Evaluation metrics
library(Metrics)

# 1.0 ERAI wind data Oct-2011 ----

# * Load the data ----

ncdata <- open.nc("00_data/ERA-Interim Oct 2011.nc")

print.nc(ncdata)

# Retrieve number of recording time
time <- var.get.nc(ncdata, "time")

length(time)

# * Identify grid point location ----

lon <- var.get.nc(ncdata, "longitude")

lat <- var.get.nc(ncdata, "latitude")

buoy_lon <- -3.04

buoy_lat <- 43.63

identified_lon <- which.min(abs(lon - buoy_lon))

identified_lat <- which.min(abs(lat - buoy_lat))

min_lon <- lon[identified_lon]

min_lat <- lat[identified_lat]

dist <- distance(buoy_lat, min_lat, buoy_lon, min_lon)

dist # Given distance in km

# * Extract wind components ----

# Zonal component
U <- var.get.nc(ncdata, "u10",
  start = c(identified_lon, identified_lat, 1),
  count = c(1, 1, length(time)), unpack = TRUE
)

# Meridional component
V <- var.get.nc(ncdata, "v10",
  start = c(identified_lon, identified_lat, 1),
  count = c(1, 1, length(time)), unpack = TRUE
)

# * ERAI wind model ----

windModel <- sqrt(U^2 + V^2)

summary(windModel)

length(windModel)

# 2.0 Bilbao-Vizcaya buoy data ----

# * Load the data ----

buoydata <- file.path("00_data/BilbaoVizcaya.txt")

buoydata <- read.table(buoydata, header = TRUE)

buoydata_oct2011 <- buoydata %>%
  filter(buoydata$AA == "2011" &
    buoydata$MM == "10")

buoydata_oct2011

# * Match both wind data ----

# Match wind data from buoy with 6-hourly sequence of ERAI
seq_6h <- seq(1, nrow(buoydata_oct2011), 6)

seq_6h

windSpeed <- buoydata_oct2011[seq_6h, 22] # Column 22 (Vv_md)

windSpeed

# * Transform wind speed to 10 m height ----

windSpeed10 <- windSpeed * log(10 / 0.0005) / log(3 / 0.0005)

summary(windSpeed10)

# * Verify the length of wind data ----

length(windSpeed10) # Bibao-Vizcaya buoy

length(windModel) # ERA-Interim

# * Plot the both wind data ----

plot(c(1:length(windSpeed10)), windModel,
  type = "l",
  main = "Wind data from buoy and ERAI model",
  xlab = "Number of records",
  ylab = "Wind speed (m/s)",
  col = "blue"
)

lines(windSpeed10, col = "red")

legend("topleft",
  col = c("red", "blue"),
  lty = 1,
  legend = c("Wind data from buoy", "Wind data from ERAI model")
)


# 3.0 Box plot and wind rose ----

# * Box plot ----

boxplot(windSpeed10, windModel,
  col = c("red", "blue"),
  names = c("Bilbao-Vizcaya buoy", "ERA-Interim"),
  ylab = "Wind speed (m/s)",
  main = "Wind data (speed) comparison for Oct-2011 records"
)

# * Wind rose ----

# Bilbao-Vizcaya buoy
year <- buoydata_oct2011$AA

month <- buoydata_oct2011$MM

day <- buoydata_oct2011$DD

hour <- buoydata_oct2011$HH

wind_spd <- buoydata_oct2011$Vv_md

wind_dir <- buoydata_oct2011$Dv_md

wind_df <- data.frame(year, month, day, hour, wind_spd, wind_dir)

wind_df

windRose(wind_df,
  ws = "wind_spd",
  wd = "wind_dir",
  ws.int = 4,
  angle = 22.5,
  cols = "hue",
  paddle = FALSE
)

# ERA-Interim (option 1)
wind_df_erai <- as.data.frame(uv2ds(-U, -V))

names(wind_df_erai)[1] <- "Wind Direction"

names(wind_df_erai)[2] <- "Wind Speed"

wind_df_erai

windRose(wind_df_erai,
  ws = "Wind Speed",
  wd = "Wind Direction",
  ws.int = 4,
  angle = 22.5,
  cols = "hue",
  paddle = FALSE
)

# ERA-Interim (option 2); a user-defined function
uv2wdws <- function(u, v) {

  # Radians to degrees transformation
  degrees <- function(radians) 180 * radians / pi

  # Apply degrees() to convert radians to degrees; use atan2() inside degrees()
  # to determine the angle from both wind components
  angle <- degrees(atan2(v, u))

  # Fix negative angles from the previous section and change the wind rose
  # convention to show from which direction the wind comes from
  wind_dir_fix <- ifelse(angle > 0, angle, angle + 360)

  wind_dir <- ifelse(wind_dir_fix < 270,
    270 - wind_dir_fix, 270 - wind_dir_fix + 360
  )

  wind_spd <- sqrt(u^2 + v^2)

  return(cbind(wind_dir, wind_spd))
}

wind_df_erai <- as.data.frame(uv2wdws(U, V))

names(wind_df_erai)[1] <- "Wind Direction"

names(wind_df_erai)[2] <- "Wind Speed"

wind_df_erai

windRose(wind_df_erai,
  ws = "Wind Speed",
  wd = "Wind Direction",
  ws.int = 4,
  angle = 22.5,
  cols = "hue",
  paddle = FALSE
)

# 4.0 Taylor diagram ----

# * Wind speed data ----

taylor.diagram(windSpeed10, windModel,
  pch = 19,
  pos.cor = TRUE,
  xlab = "Standard deviation",
  ylab = "Standard deviation",
  main = "Taylor Diagram",
  ngamma = 10,
  gamma.col = "green",
  sd.arcs = TRUE,
  pcex = 1.5
)

# * Evaluate the taylor diagram ----

# Correlation value
cor(windSpeed10, windModel)

# Standard deviation of buoy data
sd(windSpeed10)

# Standard deviation of ERA-Interim model
sd(windModel)

# RMSE
rmse(windSpeed10, windModel)

# * Wind speed components ----

# Match wind data from buoy with 6-hourly sequence of ERAI
windDirection <- buoydata_oct2011[seq_6h, 23] # Column 23 (Dv_md)

windDirection

# Create a user-defined function
wdws2uv <- function(wd, ws) {

  # Adjust the angles to avoid negative cos & sin values
  wd_u <- ifelse(wd > 180 & wd < 360, wd + 180, wd)
  wd_v <- ifelse(wd > 90 & wd < 270, wd + 180, wd)
  wd_u_ang <- ifelse(wd_u > 360, wd_u - 360, wd_u)
  wd_v_ang <- ifelse(wd_v > 360, wd_v - 360, wd_v)

  # Convert angles to radians
  wd_u_rad <- wd_u_ang * (pi / 180)
  wd_v_rad <- wd_v_ang * (pi / 180)

  # Calculating components according to the meteorology wind convention
  U <- ws * sin(wd_u_rad)
  V <- ws * cos(wd_v_rad)

  return(cbind(U, V))
}

wind_uv_components <- as.data.frame(wdws2uv(windDirection, windSpeed10))

wind_uv_components

# Taylor diagram for wind zonal component from buoy vs
# ERAI wind speed (combined U & V)
taylor_diagram_U <- taylor.diagram(wind_uv_components[, 1], windModel,
  pch = 19,
  pos.cor = TRUE,
  xlab = "Standard deviation",
  ylab = "Standard deviation",
  main = "Taylor Diagram (zonal component)",
  ngamma = 10,
  gamma.col = "green",
  sd.arcs = TRUE,
  pcex = 1.5
)

# Taylor diagram for wind meridional component from buoy vs
# ERAI wind speed (combined U & V)
taylor_diagram_V <- taylor.diagram(wind_uv_components[, 2], windModel,
  pch = 19,
  pos.cor = TRUE,
  xlab = "Standard deviation",
  ylab = "Standard deviation",
  main = "Taylor Diagram (meridional component)",
  ngamma = 10,
  gamma.col = "green",
  sd.arcs = TRUE,
  pcex = 1.5
)
