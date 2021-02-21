# LIBRARIES ----

# Quickly visualizing data
library(visdat)

# Working with NA's
library(naniar)

# Simple imputation
library(simputation)
library(randomForest)

# Manipulating and analysing data
library(tidyverse)

# Wind rose plot
library(openair)

# 1.0 Bilbao-Vizcaya buoy data ----

# * Load the data ----

buoydata <- file.path("00_data/BilbaoVizcaya.txt")

buoydata <- read.table(buoydata, header = TRUE)

buoydata <- buoydata %>%
  filter(buoydata$AA > "2009")

summary(buoydata)

# * Wind speed and direction ----

year <- buoydata$AA

month <- buoydata$MM

day <- buoydata$DD

hour <- buoydata$HH

wind_spd <- buoydata$Vv_md

wind_dir <- buoydata$Dv_md

wind_df <- data.frame(year, month, day, hour, wind_spd, wind_dir)

wind_df_tbl <- wind_df %>% as_tibble()

wind_df_tbl

# 2.0 Missing data visualization ----

# * vis_dat() -----

wind_df_tbl %>% vis_dat()

# * vis_miss() ----

wind_df_tbl %>% vis_miss()

# * gg_miss_upset() ----

wind_df_tbl %>% gg_miss_upset()

# * geom_miss_point() ----

wind_df_tbl %>%
  ggplot(aes(x = wind_dir, y = wind_spd)) +
  geom_miss_point()

# 3.0 Missing data imputation ----

# * Linear Imputation - impute_lm() ----

wind_df_tbl %>%

  # Label if wind_spd is missing
  add_label_missings(wind_dir) %>%

  # Imputation - Linear Regression
  impute_lm(wind_dir ~ wind_spd + month) %>%

  # Visualize
  ggplot(aes(wind_dir, wind_spd, color = any_missing)) +
  geom_point()

# * Random Forest - impute_rf() ----

wind_df_tbl %>%

  # Label if wind_spd is missing
  add_label_missings(wind_dir) %>%

  # Imputation - Random Forest
  impute_rf(wind_dir ~ wind_spd + month) %>%

  # Visualize
  ggplot(aes(wind_dir, wind_spd, color = any_missing)) +
  geom_point()

# * Save the result as object ----

wind_df_tbl_fixed <- wind_df_tbl %>%

  # Label if wind_spd is missing
  add_label_missings(wind_dir) %>%

  # Imputation - Random Forest
  impute_rf(wind_dir ~ wind_spd + month)

# 4.0 Wind rose plot ----

# * Plot for winter wind data ----

winter_df <- wind_df_tbl_fixed %>%
  filter(wind_df_tbl_fixed$month == 12 |
    wind_df_tbl_fixed$month == 1 |
    wind_df_tbl_fixed$month == 2) # DEC-JAN-FEB

winter_df

windRose(winter_df,
  ws = "wind_spd",
  wd = "wind_dir",
  ws.int = 4,
  angle = 22.5,
  cols = "hue",
  paddle = FALSE
)

# * Plot for summer wind data ----

summer_df <- wind_df_tbl_fixed %>%
  filter(wind_df_tbl_fixed$month == 6 |
    wind_df_tbl_fixed$month == 7 |
    wind_df_tbl_fixed$month == 8) # JUN-JUL-AUG

summer_df

windRose(summer_df,
  ws = "wind_spd",
  wd = "wind_dir",
  ws.int = 4,
  angle = 22.5,
  cols = "hue",
  paddle = FALSE
)
