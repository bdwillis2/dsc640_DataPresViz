# Version: 4
library(dplyr)
library(readxl)
library(reshape2)

# This is only really used actively when I was doing testing the terminal
options('width'=270)

# Quicvk function to read and  have smiilar transforms to normal set
apd_fatality_csv = function(file_name){
  # Reading base file
  base_df = read.csv(file_name, na.strings="n/a")
  # Using filename instead of date field in data since they switch representations/format many times
  base_df$year = strsplit(basename(file_name),"_")[[1]][1]
  base_df$transportation_type = "motor_vehicles"
  base_df = base_df %>% mutate(month=case_when(
    Month == "Jan" ~ "January",
    Month == "Feb" ~ "February",
    Month == "Mar" ~ "March",
    Month == "Apr" ~ "April",
    Month == "May" ~ "May",
    Month == "Jun" ~ "June",
    Month == "Jul" ~ "July",
    Month == "Aug" ~ "August",
    Month == "Sep" ~ "September",
    Month == "Oct" ~ "October",
    Month == "Nov" ~ "November",
    Month == "Dec" ~ "December",
    Month == "DEC" ~ "December",
  ))
  # Making copies to transform a bit
  incidents_df = base_df
  fatalities_df = base_df
  incidents_df$incident_type = "fatal_accidents"
  fatalities_df$incident_type = "fatalities"
  incidents_df$value = 1
  fatalities_df$value = fatalities_df$"Number.of.Fatalities"
  return(bind_rows(incidents_df,fatalities_df))
}

# Could be done using dyplr commands in many instances but did this incrementally
# to keep it legible and because that is how I was looking at the data at that time

# This is the initial dataset that is required
airline_safety = read_excel("raw_data/airline-safety.csv")

# Here are more supporting data
base_crash_fatality_2012 = read_excel("raw_data/2012-apd-crash-fatality-data-1.csv") # Austin texas data https://data.austintexas.gov/w/ergh-7g8p/7r79-5ncn?cur=rK4gDgqgsaG
crash_report = read_excel("raw_data/CrashReport.csv", sheet="crashes_by_year") # National Highway Traffic Safety Administration (NHTSA) Motor Vehicle Crash Data Querying and Reporting
monroe_county_crash = read_excel("raw_data/monroe-county-crash-data2003-to-2015.csv") # Crash data for single county in Indiana (https://catalog.data.gov/dataset/traffic-data/resource/e46a5cc5-ed4d-4b8d-b750-18e6c9ec570e)
# All of these are apd from data.gov
apd_fatality_2013 = apd_fatality_csv("raw_data/2013_APD_Traffic_Fatalities.csv")
apd_fatality_2014 = apd_fatality_csv("raw_data/2014_APD_Traffic_Fatalities.csv")
apd_fatality_2015 = apd_fatality_csv("raw_data/2015_APD_Traffic_Fatalities.csv")
apd_fatality_2016 = apd_fatality_csv("raw_data/2016_APD_Traffic_Fatalities.csv")
apd_fatality_2017 = apd_fatality_csv("raw_data/2017_APD_Traffic_Fatalities.csv")
apd_fatality_2018 = apd_fatality_csv("raw_data/2018_APD_Traffic_Fatality_Data_021219.csv")
apd_fatality_2019 = apd_fatality_csv("raw_data/2019_APD_Traffic_Fatality_Data.csv")

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
airline_safety$source = "airline_safety"

# Transforming supporting data sets
base_crash_fatality_2012$year = format(base_crash_fatality_2012$Date, format="%Y")
base_crash_fatality_2012$Date = as.character(base_crash_fatality_2012$Date)
base_crash_fatality_2012$transportation_type = "motor_vehicles"
incidents_crash_fatality_2012 = base_crash_fatality_2012
fatalities_crash_fatality_2012 = base_crash_fatality_2012
incidents_crash_fatality_2012$incident_type = "fatal_accidents"
incidents_crash_fatality_2012$value = 1
fatalities_crash_fatality_2012$incident_type = "fatalities"
fatalities_crash_fatality_2012$value = 1
incidents_crash_fatality_2012$source = "apd_crash"
fatalities_crash_fatality_2012$source = "apd_crash"

crash_report = melt(crash_report, id.vars="Year")
crash_report$year = as.character(crash_report$Year)
crash_report$transportation_type = "motor_vehicles"
crash_report$incident_type = "fatal_accidents"
crash_report_fatalities = crash_report
crash_report_fatalities$incident_type = "fatalities"
crash_report$source = "fars"
crash_report_fatalities$source = "fars"

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
), Day=as.character(Day), Month=as.character(Month))
monroe_county_crash$transportation_type = "motor_vehicles"
monroe_county_crash$source = "monroe_county"

# Few changes to the fatality data overall to make sure it's easier in Tableau to operate on
apd_fatality_df = bind_rows(
  apd_fatality_2013,
  apd_fatality_2014,
  apd_fatality_2015,
  apd_fatality_2016,
  apd_fatality_2017,
  apd_fatality_2018,
  apd_fatality_2019
)

apd_fatality_df$source = "apd_crash"

# Now start mangling it together
final_df = bind_rows(
  airline_safety,
  crash_report,
  crash_report_fatalities,
  monroe_county_crash,
  incidents_crash_fatality_2012,
  fatalities_crash_fatality_2012,
  apd_fatality_df
)

# Going to prune some similar columns we are going to need and to make Tableau less confusing since it is case insensitive with names
final_df[ c("Year", "X.COORD", "Y.COORD", "FATAL.CRASH..", "CASE.NUMBER",
  "LOCATION", "AREA", "XCOORD", "YCOORD", "COORD.X", "X.coord", "Y.coord", "Victim",
  "Failure.to.stop.and.render.aid", "Fatal.Crash.Number", "Case.Status", "DL.Status.incident",
  "Weekend?", "Month"
) ] = NULL

# Going to optimize the structure a bit to make sure I get those that are actually characters
final_df$year = as.integer(final_df$year)
final_df <- final_df %>% mutate_if(is.character,as.factor)

write.csv(file="visualization_data_presentation.csv", final_df, row.names=FALSE, na="")
# save(final_df, file="visualization_data_presentation.RData")
