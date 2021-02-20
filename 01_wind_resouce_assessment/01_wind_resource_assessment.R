# LIBRARIES ----

# World map data from Natural Earth
library(rnaturalearth)
library(rnaturalearthdata)

# Data visualization
library(ggplot2)
library(ggspatial)

# Data manipulation
library(dplyr)

# Statistical function
library(MASS)

# 1.0 Bilbao-Vizcaya buoy location ----

# * Load the spatial data ----

spdf_world <- ne_countries(scale = "medium", returnclass = "sf")

sldf_world <- ne_coastline(scale = "medium", returnclass = "sf")

# * Plot the corresponding data ----

theme_set(theme_bw())

ggplot() +
  geom_sf(data = spdf_world) +
  geom_sf(data = sldf_world, size = 0.75) +
  annotation_scale(width_hint = 0.5, location = "bl") +
  annotation_north_arrow(
    location = "tl",
    height = unit(1.5, "cm"),
    width = unit(1.5, "cm"),
    pad_x = unit(0.25, "cm"),
    pad_y = unit(0.25, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  annotate(
    geom = "text",
    x = -6,
    y = 46,
    label = "Bay of Biscay",
    fontface = "italic",
    color = "darkblue",
    size = 4
  ) +
  annotate(
    geom = "point",
    x = -3.04,
    y = 43.63,
    colour = "red",
    size = 2
  ) +
  annotate(
    geom = "text",
    x = -3,
    y = 43.85,
    label = "Bilbao-Vizcaya buoy",
    size = 3
  ) +
  coord_sf(xlim = c(-12, 2), ylim = c(40, 50), expand = FALSE) +
  theme(
    panel.grid.major = element_line(
      color = gray(0.5), linetype = "dashed", size = 0.5
    ),
    panel.background = element_rect(fill = "aliceblue")
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Bay of Biscay")

# 2.0 The average wind speed ----

# * Load the wind data ----

buoydata <- file.path("00_data/BilbaoVizcaya.txt")

buoydata <- read.table(buoydata, header = TRUE)

buoydata <- buoydata %>%
  filter(buoydata$AA > "2009") # To obtain the best records and to avoid a whole-year missing data

summary(buoydata)

# * Replace NA values ----

windSpeed <- na_if(buoydata$Vv_md, "NAN")

windSpeed[is.na(windSpeed)] <- mean(windSpeed, na.rm = TRUE)

summary(windSpeed)

# * Transform wind speed to 70 m height ----

windSpeed70 <- windSpeed * log(70 / 0.0005) / log(3 / 0.0005)

summary(windSpeed70)

# 3.0 Weibull distribution fit ----

# * Get Weibull parameters ----

windSpeed70[windSpeed70 == 0] <- 0.0001 # Weibull values must be > 0

summary(windSpeed70)

k <- coef(
  fitdistr(windSpeed70, "weibull")
)[1] # Shape parameter

k

c <- coef(
  fitdistr(windSpeed70, "weibull")
)[2] # Scale parameter

c

# * Plot wind data histogram ----

qVector <- seq(0, 30, 1) # Vector of quantiles

weibulldistr <- dweibull(qVector, k, c)

histogram <- hist(windSpeed70,
  qVector,
  main = "Distribution of estimated wind speeds at 70 m height",
  xlab = "Wind speed (m/s)",
  ylim = c(0, 5000)
)

plot(histogram,
  freq = FALSE,
  col = "lightsteelblue",
  main = "Wind speeds distribution (Weibull) fit",
  xlab = "Wind speed (m/s)",
  ylim = c(0, 0.14)
)

lines(qVector, weibulldistr, type = "b", lwd = 5, col = "red")

# * Get the turbine working hours ----

F3 <- 1 - exp(-((3 / c)^k))

F25 <- 1 - exp(-((25 / c)^k))

workinghrs <- (F25 - F3) * 365 * 24 # Considering 365 days per year

workinghrs

# 4.0 Initial AEP and CF ----

# * Specify turbine's data ----

eff <- 0.4

R <- 92 / 2

A <- pi * R^2

# * Inspect air pressure data ----

Ps <- na_if(buoydata$Ps, "NAN")

Ps[is.na(Ps)] <- mean(Ps, na.rm = TRUE)

Ps <- Ps * 10^2 # Pressure unit conversion from mbar to Pa

summary(Ps)

# * Inspect air temperature data ----

Ta <- na_if(buoydata$Ta, "NAN")

Ta[is.na(Ta)] <- mean(Ta, na.rm = TRUE)

Ta <- Ta + 273 # Temperature unit conversion from Celcius to Kelvin

summary(Ta)

# * Calculate initial AEP ----

rho <- Ps / (287.058 * Ta) # Air density in kg/m^3

AEP0 <- eff * 0.5 * mean(rho) * A * c^3 * gamma(1 + (3 / k)) * (365 * 24)

(AEP0 <- AEP0 * 10^-9) # AEP unit conversion from Wh to GWh

# * Calculate initial CF ----

CF0 <- AEP0 / (2.4 * (365 * 24) * 10^-3)

CF0

# 5.0 Wind power density ----

# * Plot the monhtly air density ----

monthlyAirDensity <- cbind(buoydata[, 1:4], rho)

boxplot(monthlyAirDensity[, 5] ~ monthlyAirDensity[, 2],
  col = rainbow(12),
  xlab = "Months",
  ylab = "Air density"
)

title("Monthly air density")

# * Calculate wind power density ----

WPD <- 0.5 * rho * (windSpeed70^3)

summary(WPD)

# 6.0 Actual AEP and CF ----

# * Incorporate power curve data ----

PC <- file.path("00_data/PC_MWT92-2.4.txt")

PC <- read.table(PC, header = TRUE) # Wind turbine power curve

density <- histogram$density # Sum of pdf must be equal to 1

ntimes <- length(density) - nrow(PC) # as a second argument in rep() to add extra elements (as many as n-times) in PC

dataPC <- c(PC[, 2], rep(0, ntimes)) # Size of dataPC equals to length of density: 30 elements

dataPC

# * Calculate actual AEP ----

AEP <- sum(density * dataPC) * (365 * 24) * 10^-6

AEP

# * Calculate actual CF limit ----

CF1 <- AEP / (2400 * 10^-6 * (365.25 * 24))

CF1

CF2 <- 0.087 * mean(windSpeed70) - (2400 / 92^2)

CF2
