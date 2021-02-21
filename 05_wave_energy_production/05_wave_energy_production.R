# LIBRARIES ----

# World map data from Natural Earth
library(rnaturalearth)
library(rnaturalearthdata)

# Data visualization
library(ggplot2)
library(ggspatial)

# Manipulating and analysing data
library(tidyverse)

# 1.0 Mutriku WEC location ----

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
    x = -2.3781,
    y = 43.312,
    colour = "blue",
    pch = 17,
    size = 5
  ) +
  annotate(
    geom = "text",
    x = -2.35,
    y = 43.65,
    label = "Mutriku WEC",
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
  ggtitle("Mutriku WEC location")

# 2.0 Generated wave energy ----

# * Load the data ----

energy <- file.path("00_data/KWH_2015.txt")

energy <- read.table(energy, header = TRUE)

summary(energy)

# * Energy injected to the grid ----

energy_Max <- max(energy$cumulated_kwh, na.rm = TRUE) # Maximum energy generation in kWh

energy_Min <- min(energy$cumulated_kwh, na.rm = TRUE) # Minimum energy generation in kWh

energy_Inj <- energy_Max - energy_Min

energy_Inj # Total energy injected (in kWh) to the grid in 2015

# * WEC working hours ----

workinghrs <- sum(!is.na(energy$kwh))

workinghrs # Annual working hours in 2015

percentage <- 100 * workinghrs / 8760

round(percentage, 2) # Annual working hours (in %) in 2015

# 3.0 Monthly wave energy production ----

# * Average hourly EP ----

energy_Avg <- energy_Inj / workinghrs

energy_Avg

# * Box plot ----

ggplot(energy, aes(x = factor(month), y = kwh, fill = factor(month))) +
  geom_boxplot(notch = TRUE, outlier.alpha = 0.1, varwidth = TRUE) +
  geom_hline(
    yintercept = energy_Avg,
    linetype = "dashed",
    color = "blue",
    size = 0.75
  ) +
  geom_text(
    x = 3,
    y = 150,
    color = "blue",
    label = paste("Average hourly EP = ",
      round(energy_Avg, 2), " kWh",
      sep = ""
    )
  ) +
  labs(
    x = "Month",
    y = "Hourly energy production (kWh)",
    title = "Monthly distribution of wave energy production"
  ) +
  scale_fill_discrete(
    name = "Month",
    labels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
  )

# 4.0 Wave energy flux ----

# * Load the data ----

buoydata <- file.path("00_data/BB_2015.txt")

buoydata <- read.table(buoydata, header = TRUE)

summary(buoydata)

# * Calculate WEF ----

wef <- 0.488 * ((buoydata$Hm0_m)^2 * buoydata$Tm02_s)

buoydataNew <- cbind(buoydata, wef)

buoydataNew

wef_Avg <- mean(buoydataNew$wef, na.rm = TRUE)

wef_Avg

# * Box plot ----

# Initial box plot
ggplot(buoydataNew, aes(x = factor(MM_m), y = wef, fill = factor(MM_m))) +
  geom_boxplot(notch = TRUE, outlier.alpha = 0.1, varwidth = TRUE) +
  geom_hline(
    yintercept = wef_Avg,
    linetype = "dashed",
    color = "blue",
    size = 0.75
  ) +
  geom_text(
    x = 3,
    y = 300,
    color = "blue",
    label = paste("Average WEF = ", round(wef_Avg, 2), " kW/m", sep = "")
  ) +
  labs(
    x = "Month",
    y = "Energy transport or WEF (kW/m)",
    title = "WEF from Bilbao-Vizcaya buoy data in 2015"
  ) +
  scale_fill_discrete(
    name = "Month",
    labels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
  )

# Log scale box plot
ggplot(buoydataNew, aes(x = factor(MM_m), y = log(wef + 1), fill = factor(MM_m))) +
  geom_boxplot(notch = TRUE, outlier.alpha = 0.1, varwidth = TRUE) +
  geom_text(
    x = 3,
    y = 5.5,
    color = "blue",
    label = paste("Average WEF = ", round(wef_Avg, 2), " kW/m", sep = "")
  ) +
  labs(
    x = "Month",
    y = "Wave power flux in logarithmic scale",
    title = "WEF from Bilbao-Vizcaya buoy data in 2015"
  ) +
  scale_fill_discrete(
    name = "Month",
    labels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
  )

# 5.0 Air pressure variation ----

# * Load the data ----

press <- file.path("00_data/PRES_2015.txt")

press <- read.table(press, header = TRUE)

summary(press)

# * Average air pressure ----

press_Avg <- colMeans(press[, 5:20], na.rm = TRUE)

press_Avg # Average air pressure in each turbine's chamber in Pa

press_Avg_2 <- mean(press_Avg, na.rm = TRUE)

press_Avg_2 # Average air pressure of all working turbines in Pa

# * Box plot ----

boxplot(press[, 5:20],
  col = "orange",
  border = "brown",
  xlab = "Turbine(s)", ylab = "Air pressure (Pa)", xaxt = "n",
  main = "Air pressure in each turbine's chamber in 2015",
  notch = TRUE,
  varwidth = TRUE
)

axis(1, at = seq(1, 16, 1), labels = names(press[5:20]), cex.axis = 0.6)

segments(0, press_Avg_2, 17, press_Avg_2, lwd = 2, lty = 2, col = "blue")

addedText3 <- paste("Average air pressure (in one year) = ",
  round(press_Avg_2, 2), " Pa",
  sep = ""
)

text(11, 19000, addedText3, font = 1, col = "blue")

# 6.0 Power production of each chamber ----

# * Load the data ----

power5 <- file.path("00_data/POW_2015.txt")

power5 <- read.table(power5, header = TRUE)

power5_kW <- power5[, 5:20] / 1000 # Convert the unit into kW

summary(power5_kW)

# * Average power production ----

power5_Avg <- colMeans(power5_kW, na.rm = TRUE)

power5_Avg # Average power produced of each turbine in kW

power5_Avg_2 <- mean(power5_Avg, na.rm = TRUE)

power5_Avg_2 # Average power produced of all working turbines in kW

# * Box plot ----

boxplot(power5_kW,
  col = "red",
  border = "brown",
  xlab = "Turbine(s)", ylab = "Power production (kW)", xaxt = "n",
  main = "Power production of each turbine",
  notch = TRUE, 
  varwidth = TRUE
)

axis(1, at = seq(1, 16, 1), labels = names(press[5:20]), cex.axis = 0.6)

segments(0, power5_Avg_2, 17, power5_Avg_2, col = "blue", lwd = 2, lty = 2)

addedtext4 <- paste("Average power production = ",
  round(power5_Avg_2, 2), " kW (per 5 min. record)",
  sep = ""
)

text(4, 15, addedtext4, font = 1, col = "blue")

# 7.0 Average valve opening degree ----

# * Load the data ----

posValve <- file.path("00_data/POSVALV_2015.txt")

posValve <- read.table(posValve, header = TRUE)

summary(posValve)

# * Valve opening degree T10 ----

plot(press[, 14], posValve[, 14],
  ylim = c(0, 90), xlab = "Air pressure (Pa)",
  ylab = "Opening degree (degrees)", main = "Turbine 10"
)

pressValve10 <- cbind(press[, 14], posValve[, 14])

degMax10 <- pressValve10[pressValve10[, 2] > 80, ]

pressMax10 <- max(degMax10[, 1], na.rm = TRUE)

pressMax10 # Maximum air pressure comes when the valve opening degree > 80 deg in Pa

segments(pressMax10, 0, pressMax10, 90, col = "blue", lwd = 2, lty = 2)

# * Valve opening degree T15 ----

plot(press[, 19], posValve[, 19],
  ylim = c(0, 90), xlab = "Air pressure (Pa)",
  ylab = "Opening degree (degrees)", main = "Turbine 15"
)

pressValve15 <- cbind(press[, 19], posValve[, 19])

degMax15 <- pressValve15[pressValve15[, 2] > 80, ]

pressMax15 <- max(degMax15[, 1], na.rm = TRUE)

pressMax15 # Maximum air pressure comes when the valve opening degree > 80 deg in Pa

segments(pressMax15, 0, pressMax15, 90, col = "blue", lwd = 2, lty = 2)

# 8.0 Plant's efficiency index (PEI) ----

# Calculate PEI ----

power5_Avg_3 <- rowMeans(power5_kW, na.rm = TRUE) # Average power produced in kW per 5 minutes

PEI <- power5_Avg_3 / buoydataNew[, 7] # buoydataNew[, 7] represents wef estimation data from buoy

PEI <- na_if(PEI, "NaN")

PEI <- PEI[PEI < 0.6] # To limit the measured PEI

max(PEI, na.rm = TRUE)

min(PEI, na.rm = TRUE)

mean(PEI, na.rm = TRUE)

hist(PEI,
  xlim = c(0, 0.6), breaks = seq(0, 1, 0.025),
  freq = TRUE, col = "lightgrey", ylim = c(0, 80),
  main = "Plant's efficiency index (PEI) distribution"
)

segments(0.255, 0, 0.255, 80, col = "blue", lwd = 2, lty = 2)

text(0.35, 75, "Average PEI = 0.255", col = "blue", font = 1)

# Box plot ----

PEI_df <- data.frame(cbind(PEI, power5$month))

PEI_df

ggplot(PEI_df, aes(x = factor(V2), y = PEI, fill = factor(V2))) +
  geom_boxplot(outlier.alpha = 0.1, varwidth = TRUE) +
  labs(
    x = "Month",
    y = "Plant's efficiency index (PEI)",
    title = "Plant's efficiency index (PEI) of Mutriku WEC in 2015"
  ) +
  scale_fill_discrete(
    name = "Month",
    labels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
  )
