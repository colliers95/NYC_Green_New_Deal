# fp1 = file.path(
#   "C:",
#   "Users",
#   "Sam",
#   "Desktop",
#   "NYC Data Science bootcamp",
#   "R",
#   "Shiny",
#   "NYC_Green_New_Deal",
#   "data",
#   "nyc_benchmarking_disclosure_data_reported_in_2017-2.csv"
# )
# buildings = read.csv(fp1, header = TRUE)
# #####NOTES#####
# ## Pretty much all of the outliers are either offices, hotels or multifamily housing
# ## Major emitters are the offices and hotels, funnily enough also the major anti-lobbiers!
# ##
#
# ## Check the scatter plots of relevant variables look reasonable
# library(ggplot2)
# library(dplyr)
# library(googleVis)
# buildings = buildings %>% select(
#   borough = Borough,
#   gross_sf = DOF.Gross.Floor.Area,
#   year = Year.Built,
#   total_gg_MtCO2e = Total.GHG.Emissions..Metric.Tons.CO2e.,
#   property_type = Primary.Property.Type...Self.Selected,
#   property_name = Property.Name
# ) %>% mutate(property_name = tolower(as.character(property_name)))
#
# # Opening plot of indicative emissions changes in NY
# # Energy use in buildings is Residential, Commercial and Industrial fossil fuel combustion, plus electricity
# co2_emissions_changes = data.frame(year = c('2015', '2030'), Buildings = c(97.4, 58.4), Transportation = c(72.8, 47.7), Waste = c(13.2, 8.7), Industry = c(12.2, 8.0), Agriculture = c(8.9, 5.8))
# plot(gvisColumnChart(co2_emissions_changes, options = list(isStacked = "true")))
#
#
#


# Converting from the Sate Plane Coordinates used in the bus data, to Latitude and Longitude data for plotting
# nad83_coords = unique(buses %>% select(XCoordinates, YCoordinates))

# Plan of attack. Use data.frame(coordinates(bus_garage_coords)), check the class of the two columns (change to numeric if not already),
# join this with the existing data frame and then group by to find the number of buses at each depo. If this doesn't work, try without the unique and hope the operations preserve order, then do the group by.

str(pollutants)
library(rgdal)
#nycounties <- readOGR("gz_2010_us_050_00_500k", "OGRGeoJSON")


