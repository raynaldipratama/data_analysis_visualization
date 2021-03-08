# LIBRARIES ----

# Extreme values
library(ismev)
library(evir)

# 1.0 Bilbao-Vizcaya buoy data ----

# * Load the data ----

buoydata <- file.path("00_data/BilbaoVizcaya.txt")

buoydata <- read.table(buoydata, header = TRUE)

summary(buoydata)

# * Annual maximum wind speed ----

windSpeedMax <- c()

for (i in 1998:2013) {
  year <- which(buoydata$AA == i)
  windSpeedMax[i - 1997] <- max(buoydata[year, 22], na.rm = TRUE)
}

windSpeedMax # Annual max win speeds from 1998 to 2013 at 3 m height

# * windSpeedMax at 70 m height ----

windSpeedMax70 <- windSpeedMax * log(70 / 0.0005) / log(3 / 0.0005)

windSpeedMax70 # Annual max win speeds from 1998 to 2013 at 70 m height

# 2.0 GEV distribution fit ----

fit <- gev.fit(windSpeedMax70)

# Negative (maximised) log–likelihood
fit$nllh

# Maximum likelihood estimates for µ, s and <U+03BE> respectively
fit$mle[1] # µ (location)

fit$mle[2] # s (scale)

fit$mle[3] # <U+03BE> (shape)

# Associated standard errors for the above parameters
fit$se

# 3.0 Expected max wind speeds ----

gevdistr <- gev(windSpeedMax70)

matA <- matrix(NaN, nrow = 7, ncol = 4) # Empty matrix

years <- c(10, 20, 30, 40, 50, 70, 100)

matA[, 1] <- years

for (i in 1:7) {
  matA[i, 2:4] <- rlevel.gev(gevdistr, years[i],
    add = FALSE,
    xlab = "Return level",
    ylab = "Profile log–likelihood"
  )
}

matA # Matrix A is presented without proper column names

rownames(matA) <- c("1", "2", "3", "4", "5", "6", "7")

colnames(matA) <- c(
  "n-years",
  "Lower limit",
  "Central value",
  "Upper limit"
)

matA # All values are in m/sec

# 4.0 Extreme operating gust ----

# * Average wind speeds ----

windSpeedAvg <- c()

for (i in 1998:2013) {
  year <- which(buoydata$AA == i)
  windSpeedAvg[i - 1997] <- mean(buoydata[year, 22], na.rm = TRUE)
}

windSpeedAvg # Annual average win speeds from 1998 to 2013 at 3 m height

# * windSpeedAvg at 70 m height ----

windSpeedAvg70 <- windSpeedAvg * log(70 / 0.0005) / log(3 / 0.0005)

windSpeedAvg70 # Annual average win speeds from 1998 to 2013 at 70 m height

# * Standard deviation ----

Iref <- 0.16

Uhub <- mean(windSpeedAvg70)

SD <- Iref * (0.75 * Uhub + 5.6)

SD

# * IEC 50-year gust ----

D <- 92 # Blade diameter in m

Ugust50 <- 6.4 * (SD / (1 + (0.1 * D / 21)))

Ugust50

T <- 14 # Time in sec

Ut <- c()

for (t in 0:15) {
  Ut[t] <- Uhub - 0.37 * Ugust50 *
    sin((3 * pi * (t - 1)) / T) *
    (1 - cos((2 * pi * (t - 1)) / T))
}

sec <- seq(0, 14, by = 1) # Time in sec

plot(sec, Ut,
  type = "b", lwd = 3, col = "red",
  xlab = "Time (sec)",
  ylab = "Wind speed (m/s)",
  main = "Extreme operating gust"
)
