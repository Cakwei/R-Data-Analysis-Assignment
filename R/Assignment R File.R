# ====
# README

# 1. Please set working directory for this project to the "R-Data-Analysis-Assignment/R/data" folder.
# 2. If you have not installed dplyr or tidyr packages for this project, please do so :)
# ====

# Uncomment the line below if you have no installed the packages
# install.packages(c("dplyr"))
library(dplyr)

# ==== Beginning of the actual code ====
airQuality <- read.csv("2. Beijing_AirQuality.csv")

# List all columns in dataset
colnames(airQuality)

# Check all row for "No" column
which(grepl("[^0-9]", airQuality$No))

# Check all row for "year" column
# levels(factor(airQuality$year, exclude=FALSE))
table(airQuality$year, exclude = FALSE)
length(unique(airQuality$year))

# Check all row for "month" column
# levels(factor(airQuality$month, exclude=FALSE))
table(airQuality$month, exclude = FALSE)
length(unique(airQuality$month))

# Check all row for "day" column
# levels(factor(airQuality$day, exclude=FALSE))
table(airQuality$day, exclude = FALSE)
length(unique(airQuality$day))

# ====
# Check all row for "hour" column
# ERROR: Specifically Hour 25 should be converted 0
# FIX: Check all columns and replace hour 25 to hour 0 (New day)
airQuality <- airQuality %>%
  mutate(hour = if_else(hour == 25, 0, hour))

table(airQuality$hour, exclude = FALSE)
length(unique(airQuality$hour))
# ====

# Check all row for "PM2.5" column
# levels(factor(airQuality$PM2.5, exclude=FALSE))
# Cannot be negative based on https://pmc.ncbi.nlm.nih.gov/articles/PMC10497433/
table(airQuality$PM2.5, exclude = FALSE)
airQuality$PM2.5[airQuality$PM2.5 <= 0] <- NA
length(unique(airQuality$PM2.5))


# airQuality <- airQuality %>%
#   mutate(PM2.5 = if_else(is.na(PM2.5), 0, PM2.5)) # ADJUST Accordingly

# Check all row for "PM10" column
table(airQuality$PM10, exclude = FALSE)
length(unique(airQuality$PM10))

airQuality <- airQuality %>%
  mutate(PM10 = if_else(is.na(PM10), NA, PM10)) # ADJUST Accordingly

# Check all row for "SO2" column
table(airQuality$SO2, exclude = FALSE)
length(unique(airQuality$SO2))

# Check all row for "NO2" column
table(airQuality$NO2, exclude = FALSE)
length(unique(airQuality$NO2))

# Check all row for "CO" column
table(airQuality$CO, exclude = FALSE)
length(unique(airQuality$CO))

# Check all row for "O3" column
table(airQuality$O3, exclude = FALSE)
length(unique(airQuality$O3))


# Check all row for "TEMP" column ==================== -999 TEMPS
table(airQuality$TEMP, exclude = FALSE) # Checks first 6
tail(table(airQuality$TEMP, exclude = FALSE)) # Check last 6
airQuality$TEMP[airQuality$TEMP <= -50] <- NA
length(unique(airQuality$TEMP))

# Check all row for "PRES" column
table(airQuality$PRES, exclude = FALSE)
length(unique(airQuality$PRES))

# Check all row for "DEWP" column
table(airQuality$DEWP, exclude = FALSE)
length(unique(airQuality$DEWP))


# Check all row for "RAIN" column
table(airQuality$RAIN, exclude = FALSE)
length(unique(airQuality$RAIN))

# Check all row for "wd" column ==================== NA Exist
table(airQuality$wd, exclude = FALSE)
length(unique(airQuality$wd))
airQuality$wd <- toupper(airQuality$wd)

# Check all row for "WSPM" column
table(airQuality$WSPM, exclude = FALSE)
airQuality$WSPM[airQuality$WSPM < 0] <- NA
length(unique(airQuality$WSPM))


# Check all row for "station" column
# levels(factor(airQuality$station)) DONE
table(airQuality$station, exclude = FALSE)
airQuality$station <- tolower(airQuality$station)
unique(airQuality$station)
length(unique(airQuality$station))

# Check for duplicates (Pending approval)
# Duplicates for rows ALL SIMILAR data
airQuality <- airQuality[!duplicated(airQuality), ]
airQuality
airQuality <- airQuality[!duplicated(airQuality[, c("No","station")]), ]
airQuality
# ====

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
# Every group has 1 main hypothesis -> split to become few objectives/processes to solve the hypothesis (1 person each) and need to have 3 analysis


# Lecturer Question To Be Asked:

# Is row number unique?
# For hour 24 set to 24 or 0?



# Final Checks
nrow(airQuality)                    # expect 175320
table(airQuality$station)           # expect 35064 each
range(airQuality$hour)              # expect 0 23
sum(airQuality$TEMP <= -50, na.rm = TRUE)   # expect 0
min(airQuality$WSPM, na.rm = TRUE)  # expect >= 0
length(unique(airQuality$wd))           # expect 16 (+ NA)
duplicated(airQuality[, c("station", "datetime")])  # expect 0
