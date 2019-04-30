library(ggplot2)
library(dplyr)
library(rgdal)

fp1 = file.path(
  "C:",
  "Users",
  "Sam",
  "Desktop",
  "NYC Data Science bootcamp",
  "R",
  "Shiny",
  "NYC_Green_New_Deal",
  "data",
  "nyc_benchmarking_disclosure_data_reported_in_2017-2.csv"
)
fp2 = file.path(
  "C:",
  "Users",
  "Sam",
  "Desktop",
  "NYC Data Science bootcamp",
  "R",
  "Shiny",
  "NYC_Green_New_Deal",
  "data",
  "School bus routes.csv"
)
fp3 = file.path(
  "C:",
  "Users",
  "Sam",
  "Desktop",
  "NYC Data Science bootcamp",
  "R",
  "Shiny",
  "NYC_Green_New_Deal",
  "data",
  "Annual pollutant measurements.csv"
)
buildings = read.csv(fp1, header = TRUE)
buses = read.csv(fp2, header = TRUE, stringsAsFactors = FALSE)
pollutants = read.csv(fp3, header = TRUE, stringsAsFactors = FALSE)

# Input and format the buildings data set
buildings = buildings %>% select(
  borough = Borough,
  gross_sf = DOF.Gross.Floor.Area,
  year = Year.Built,
  total_gg_MtCO2e = Total.GHG.Emissions..Metric.Tons.CO2e.,
  property_type = Primary.Property.Type...Self.Selected,
  property_name = Property.Name
) %>% mutate(property_name = tolower(as.character(property_name)),
             gross_sf = gross_sf / 1000)


# Build the data frame for use with googleVis, which has a different schema
buildings_gVis = data.frame(buildings)
buildings_gVis$Bronx = ifelse(buildings_gVis$borough == "Bronx",
                              buildings_gVis$total_gg_MtCO2e,
                              NA)
buildings_gVis$Bronx.html.tooltip = buildings_gVis$property_name
buildings_gVis$Brooklyn = ifelse(buildings_gVis$borough == "Brooklyn",
                                 buildings_gVis$total_gg_MtCO2e,
                                 NA)
buildings_gVis$Brooklyn.html.tooltip = buildings_gVis$property_name
buildings_gVis$Manhattan = ifelse(buildings_gVis$borough == "Manhattan",
                                  buildings_gVis$total_gg_MtCO2e,
                                  NA)
buildings_gVis$Manhattan.html.tooltip = buildings_gVis$property_name
buildings_gVis$Queens = ifelse(buildings_gVis$borough == "Queens",
                               buildings_gVis$total_gg_MtCO2e,
                               NA)
buildings_gVis$Queens.html.tooltip = buildings_gVis$property_name
buildings_gVis$`Staten Island` = ifelse(buildings_gVis$borough == "Staten Island",
                                        buildings_gVis$total_gg_MtCO2e,
                                        NA)
buildings_gVis$`Staten Island.html.tooltip` = buildings_gVis$property_name
buildings_gVis$property_name = NULL

# Build a separate data frame for use with leaflet, involving a change of coordinates for the bus data
nad83_coords = unique(buses %>% select(x = XCoordinates, y = YCoordinates))
coordinates(nad83_coords) = c('x', 'y')
proj4string(nad83_coords) = CRS("+init=esri:102718")
bus_garage_coords = spTransform(nad83_coords,CRS("+init=epsg:4326"))

pollutants_by_group = ungroup(pollutants %>% group_by(Latitude, Longitude) %>% summarise(count= n()))

bus_by_garage = cbind(data.frame(coordinates(bus_garage_coords)), ungroup(buses %>% group_by(XCoordinates, YCoordinates) %>% summarise(count = n())))
