# ====
# README
# 1. Please set working directory for this project to the "R-Data-Analysis-Assignment/R/data" folder.
# 2. If you have not installed dplyr or tidyr packages for this project, please do so :)
# ====

# Uncomment the line below if you have not installed the packages
# install.packages(c("dplyr"))
library(dplyr)

# ==== Beginning of the actual code
airQuality <- read.csv("2. Beijing_AirQuality.csv")

# Initial structural of dataset
dim(airQuality)       # 183220 rows, 18 columns
colnames(airQuality)
str(airQuality)


# ====
# Standardize colnames ("wd" & "station")
# ====

# Check "wd" (Wind Direction: 16-point compass notation)
table(airQuality$wd, useNA = "ifany")
length(unique(airQuality$wd))
# Output: Different cases, not standardize colnames

# Solution: Convert "wd" to uppercase
airQuality$wd <- toupper(airQuality$wd)

# Verify "wd"
table(airQuality$wd, useNA = "ifany")
length(unique(airQuality$wd)) # Now 16 + 1 (NA) valid compass directions


# Check "station"
table(airQuality$station, useNA = "ifany")
length(unique(airQuality$station))
# Output: Non standardized naming due to different cases

# Solution: Convert "station" to lowercase
airQuality$station <- tolower(airQuality$station)

# Verify "station"
table(airQuality$station, useNA = "ifany")
length(unique(airQuality$station)) # Now 5 unique stations, all lowercase


# ====
# Check & remove duplicates
# ====
which(grepl("[^0-9]", airQuality$No))             # Check for non-digits in "No"
range(airQuality$No, na.rm = TRUE)                # Ranges from 1 to 35064 for each station
sum(duplicated(airQuality))                       # Check exact duplicate rows: 7900 found
sum(duplicated(airQuality[, c("No", "station")])) # Check duplicate (No + station) keys: 7900 found
# Output: Exactly 7900 exact duplicate rows

# Solution: Remove exact duplicates & any duplicate ("No", "station") pairs
airQuality <- airQuality[!duplicated(airQuality), ]
airQuality <- airQuality[!duplicated(airQuality[, c("No", "station")]), ]

# Verify duplicates & station row counts
nrow(airQuality)
table(airQuality$station, useNA = "ifany")
# Output: Exactly 35064 rows per station

# ====
# Check & clean datetime ("year", "month", "day", "hour")
# ====

# Check "year", "month", "day" columns
table(airQuality$year, useNA = "ifany")           # Valid years (2013 to 2017)
table(airQuality$month, useNA = "ifany")          # Valid months (1 to 12)
table(airQuality$day, useNA = "ifany")            # Valid days (1 to 31)

# Check "hour" (Hour == 25 inaccurate)
table(airQuality$hour, useNA = "ifany")
# Output:
# 25 hours: 200 rows (Should be 0 - 23 hours)
# Record logs every day, every hour

# Developer Checking Variables: Change variables to inspect a specific station and date (E.g: dingling @ 2013-03-12)
targetStation <- "dingling"
targetYear    <- 2013
targetMonth   <- 3
targetDay     <- 12
colsToShow    <- c("No", "year", "month", "day", "hour", "PM2.5", "TEMP", "station")

# Before
dayCheck_Before <- airQuality[
  airQuality$station == targetStation &
    airQuality$year  == targetYear &
    airQuality$month == targetMonth &
    airQuality$day   == targetDay,
  colsToShow
]
dayCheck_Before <- dayCheck_Before[order(dayCheck_Before$No), ]
print(dayCheck_Before, row.names = FALSE)

# Solution: Sort ascending by station and "No", and replace hour 25 with correct hour
airQuality <- airQuality[order(airQuality$station, airQuality$No), ]

airQuality <- airQuality %>%
  mutate(hour = if_else(hour == 25L, as.integer((No - 1L) %% 24L), hour))

# After
dayCheck_After <- airQuality[
  airQuality$station == targetStation &
    airQuality$year  == targetYear &
    airQuality$month == targetMonth &
    airQuality$day   == targetDay,
  colsToShow
]
dayCheck_After <- dayCheck_After[order(dayCheck_After$No), ]
print(dayCheck_After, row.names = FALSE)

# Verify "hour":
table(airQuality$hour, useNA = "ifany") # Every hour has 7,305 rows
summary(airQuality$hour)
sum(duplicated(airQuality[, c("station", "year", "month", "day", "hour")])) # 0 duplicate timestamps


# ====
# Check & clean ("PM2.5", "PM10", "SO2", "NO2", "CO", "O3") columns
# ====

# Pollutant concentrations (ug/m3) cannot be <= 0.
# PM2.5
summary(airQuality$PM2.5)
sum(airQuality$PM2.5 <= 0, na.rm = TRUE)
# Output: Min is 2.0, Max is 957.0, 3510 NAs

# PM10
summary(airQuality$PM10)
sum(airQuality$PM10 <= 0, na.rm = TRUE)
# Output: Min = 2.0, Max = 994.0, 2651 NAs
airQuality$PM10[airQuality$PM10 <= 0] <- NA

# SO2
summary(airQuality$SO2)
sum(airQuality$SO2 <= 0, na.rm = TRUE)
# Output: Min is 0.2856, Max is 500.0, 3455 NAs
airQuality$SO2[airQuality$SO2 <= 0] <- NA

# NO2
summary(airQuality$NO2)
sum(airQuality$NO2 <= 0, na.rm = TRUE)
# Output: Min is 1.0265, Max is 276.0, 6212 NAs
airQuality$NO2[airQuality$NO2 <= 0] <- NA

# CO
summary(airQuality$CO)
sum(airQuality$CO <= 0, na.rm = TRUE)
# Output: Min is 100.0, Max is 10000.0, 9844 NAs
airQuality$CO[airQuality$CO <= 0] <- NA

# O3
summary(airQuality$O3)
sum(airQuality$O3 <= 0, na.rm = TRUE)
# Output: Min is 0.2142, Max is 1071.0, 5865 NAs
airQuality$O3[airQuality$O3 <= 0] <- NA


# ====
# Check & clean ("TEMP", "PRES", "DEWP", "RAIN", "WSPM") columns
# ====

# TEMP (Celsius)
summary(airQuality$TEMP)
head(sort(airQuality$TEMP), 10)                # Check lowest values
tail(sort(airQuality$TEMP), 10)                # Check highest values
sum(airQuality$TEMP <= -50, na.rm = TRUE)      # Count sentinel values
# Output: 100 rows have TEMP == -999 (Anomaly)

# Solution: Replace TEMP <= -50 with NA
airQuality$TEMP[airQuality$TEMP <= -50] <- NA

# Verify "TEMP":
summary(airQuality$TEMP)    # Min is now -19.9 C, NAs increased from 195 to 295


# PRES (Atmospheric pressure, hPa)
summary(airQuality$PRES)
sum(airQuality$PRES <= 0, na.rm = TRUE)
# Output: Seems normal. Safeguard against physically impossible pressure drops.
airQuality$PRES[airQuality$PRES <= 0] <- NA


# DEWP (Dew point temperature in C)
summary(airQuality$DEWP)
head(sort(airQuality$DEWP), 10)
sum(airQuality$DEWP <= -50, na.rm = TRUE)
# Output: Seems normal. Safeguard against -999 sentinel errors corrupting models.
airQuality$DEWP[airQuality$DEWP <= -50] <- NA


# RAIN" (Precipitation in mm, cannot be negative)
summary(airQuality$RAIN)
sum(airQuality$RAIN < 0, na.rm = TRUE)
# Output: Min = 0.0, max = 72.5 mm, 189 NAs. Safeguard against negative rainfall.
airQuality$RAIN[airQuality$RAIN < 0] <- NA


# WSPM (Wind speed in m/s, SCALAR, cannot be -ve)
summary(airQuality$WSPM)
head(sort(airQuality$WSPM), 10)
sum(airQuality$WSPM < 0, na.rm = TRUE)
# Output: 329 rows -ve wind speeds

# Solution: Replace -ve WSPM values w/ NA
airQuality$WSPM[airQuality$WSPM < 0] <- NA

# Verify "WSPM":
summary(airQuality$WSPM)   # Min is now 0.0 m/s, NAs increased from 162 to 491


# ====
# Check & clean columns that are dependent on each other
# ====

# DEWP cannot exceed TEMP | Reference: https://www.reddit.com/r/meteorology/comments/3y5y7j/dew_point_greater_than_temperature/
# sum(airQuality$DEWP > airQuality$TEMP, na.rm = TRUE)
# Output: 0 violations (once TEMP == -999 was cleaned)

# PM2.5 & PM10 (PM10 includes PM2.5, so PM2.5 <= PM10)
invalid_pm <- !is.na(airQuality$PM2.5) & !is.na(airQuality$PM10) & (airQuality$PM2.5 > airQuality$PM10)
sum(invalid_pm)
head(airQuality[invalid_pm, c("No", "station", "PM2.5", "PM10")], 10)
# Output: 10,083 rows have PM2.5 > PM10

# Solution: Conflicting PM2.5 and PM10 values -> NA
airQuality$PM2.5[invalid_pm] <- NA
airQuality$PM10[invalid_pm]  <- NA

# Verify PM2.5 & PM10:
sum(airQuality$PM2.5 > airQuality$PM10, na.rm = TRUE) # 0 violations remaining


# =====
# Final dataset after cleaning
# =====

summary(airQuality)

# Structural Checks
nrow(airQuality)                                                            # Expect 175320
table(airQuality$station)                                                   # Expect 35064 each
range(airQuality$hour)                                                      # Expect 0 23
sum(duplicated(airQuality[, c("station", "No")]))                           # Expect 0
sum(duplicated(airQuality[, c("station", "year", "month", "day", "hour")])) # Expect 0
length(unique(airQuality$wd))                                               # Expect 16 + NA

# Meteorological & Physical Checks
sum(airQuality$TEMP <= -50, na.rm = TRUE)                                   # Expect 0
sum(airQuality$DEWP <= -50, na.rm = TRUE)                                   # Expect 0
min(airQuality$PRES, na.rm = TRUE)                                          # Expect > 0
min(airQuality$RAIN, na.rm = TRUE)                                          # Expect >= 0
min(airQuality$WSPM, na.rm = TRUE)                                          # Expect >= 0
sum(airQuality$DEWP > airQuality$TEMP, na.rm = TRUE)                        # Expect 0
sum(airQuality$PM2.5 > airQuality$PM10, na.rm = TRUE)                       # Expect 0


