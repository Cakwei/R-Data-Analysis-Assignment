# ====
# README

# 1. Please set working directory for this project to the "R-Data-Analysis-Assignment/R/data" folder.
# 2. If you have not installed dplyr or tidyr packages for this project, please do so :)
# ====

# Uncomment the line below if you have no installed the packages
# install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

# ==== Beginning of the actual code ====
airQuality <- read.csv("2. Beijing_AirQuality.csv")

# Check all row for "No" column
which(grepl("[^0-9]", airQuality$No))

# Check all row for "year" column
levels(factor(airQuality$year))

# Check all row for "month" column
levels(factor(airQuality$month))

# Check all row for "day" column
levels(factor(airQuality$day))

# ====
# Check all row for "hour" column
# ERROR: Specifically Hour 25 should be converted 0
# FIX: Check all columns and replace hour 25 to hour 0 (New day)
airQuality <- airQuality %>%
  mutate(hour = if_else(hour == 25, 0, hour))

levels(factor(airQuality$hour))
# ====

# Check all row for "PM2.5" column
levels(factor(airQuality$PM2.5, exclude=FALSE))

airQuality <- airQuality %>%
  mutate(PM2.5 = if_else(is.na(PM2.5), 0, PM2.5))

# Check all row for "PM10" column
levels(factor(airQuality$PM10))


# Check all row for "SO2 column
levels(factor(airQuality$SO2))


# Check all row for "NO2 column
levels(factor(airQuality$NO2))


# Check all row for "CO column
levels(factor(airQuality$CO))


# Check all row for "O3 column
levels(factor(airQuality$O3))


# Check all row for "station" column
airQuality <- airQuality %>%
  mutate(station = tolower(station))

levels(factor(airQuality$station))

# Quick Check below:
# airQuality[which(airQuality$hour == 25),]
# airQuality %>% arrange(year, month, day , hour)
# ====




airQuality %>%
  group_by_all() %>%
  filter(n() > 1) %>%
  ungroup()








# Required Wrangling For Col: station, wd,
# ====
# Descriptive: Factors influencing PM2.5 poll. lvl
# Diagnostic: (PM2.5, PM10, SO2, NO2, CO, O3) and meteorological conditions such as
# temperature, pressure, dew point, rainfall, and wind?
# Predictive: apply predictive models capable of estimating PM2.5 concentration
# Prescriptive: solution to this (EXEMPTED, only use assignment dataset)
# ====

# Objective:  The findings should support
# public-health advisories and inform pollution-control decisions

# Individual: Analysis Code
# Every group has 1 main hypothesis -> split to become few objectives (1 person each) and need to have 3 analysis


# Lecturer Question To Be Asked:

# Is row number unique?
# For hour 24 set to 24 or 0?



