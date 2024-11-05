# Define a function to calculate Julian Day
calculate_julian_day <- function(dttm) {
  # Extract date components
  year <- as.integer(format(dttm, "%Y"))
  month <- as.integer(format(dttm, "%m"))
  day <- as.integer(format(dttm, "%d"))
  hour <- as.numeric(format(dttm, "%H")) + as.numeric(format(dttm, "%M")) / 60 + as.numeric(format(dttm, "%S")) / 3600
  
  # Apply formula based on astronomical Julian Day calculation
  if (month <= 2) {
    year <- year - 1
    month <- month + 12
  }
  
  A <- floor(year / 100)
  B <- 2 - A + floor(A / 4)
  JD <- floor(365.25 * (year + 4716)) + floor(30.6001 * (month + 1)) + day + hour / 24 + B - 1524.5
  
  return(round(JD, 0))
}