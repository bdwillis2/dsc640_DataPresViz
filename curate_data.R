# VERSION: 1
library(dplyr)
library(readxl)
library(reshape2)
options('width'=270)

# Could be done using dyplr commands in many instances but did this incrementally
# to keep it legible and because that is how I was looking at the data at that time

# This is the initial dataset that is required
airline_safety = read_excel("raw_data/airline-safety.csv")

# Here are more supporting data
crash_fatality_2012 = read_excel("raw_data/2012-apd-crash-fatality-data-1.csv") # Austin texas data https://data.austintexas.gov/w/ergh-7g8p/7r79-5ncn?cur=rK4gDgqgsaG
crash_report = read_excel("raw_data/CrashReport.csv", sheet="crashes_by_year") # National Highway Traffic Safety Administration (NHTSA) Motor Vehicle Crash Data Querying and Reporting
monroe_county_crash = read_excel("raw_data/monroe-county-crash-data2003-to-2015.csv") # Crash data for single county in Indiana (https://catalog.data.gov/dataset/traffic-data/resource/e46a5cc5-ed4d-4b8d-b750-18e6c9ec570e)

# Now to transform the data and start mixing and mangling them
airline_safety = melt(airline_safety, id.vars=c("airline", "avail_seat_km_per_week"),
  measure.vars=c("incidents_85_99", "fatal_accidents_85_99", "fatalities_85_99", "incidents_00_14", "fatal_accidents_00_14", "fatalities_00_14") ) %>%
  rename()
# Pull out a few variables
airline_safety$incident_type = gsub("_[0-9]{2}_[0-9]{2}", "", airline_safety$variable)
airline_safety$year_strings = gsub("_", "-", gsub("[a-zA-Z]+_", "", airline_safety$variable) )
airline_safety$start_year = format(as.Date(gsub("-[0-9]{2}", "", airline_safety$year_strings), format="%y"), format="%Y")
airline_safety$end_year = format(as.Date(gsub("[0-9]{2}-", "", airline_safety$year_strings), format="%y"), format="%Y")
airline_safety$year = airline_safety$end_year
airline_safety$transportation_type = "airline"

# Transforming supporting data sets
crash_fatality_2012$year = format(crash_fatality_2012$Date, format="%Y")
crash_fatality_2012$transportation_type = "motor_vehicles"
crash_report = melt(crash_report, id.vars="Year")
crash_report$year = as.character(crash_report$Year)
crash_report$transportation_type = "motor_vehicles"
monroe_county_crash$year = as.character(monroe_county_crash$Year)
monroe_county_crash = monroe_county_crash %>% mutate(month=case_when(
  Month == 1 ~ "January",
  Month == 2 ~ "February",
  Month == 3 ~ "March",
  Month == 4 ~ "April",
  Month == 5 ~ "May",
  Month == 6 ~ "June",
  Month == 7 ~ "July",
  Month == 8 ~ "August",
  Month == 9 ~ "September",
  Month == 10 ~ "October",
  Month == 11 ~ "November",
  Month == 12 ~ "December",
), Day=as.character(Day))
monroe_county_crash$transportation_type = "motor_vehicles"

# Now start mangling it together
finalized_structure = bind_rows(
  airline_safety,
  crash_report,
  crash_fatality_2012,
  monroe_county_crash
)

write.csv(file="visualization_data.csv", finalized_structure, row.names=FALSE)
