library(ggplot2)
library(dplyr)
library(tigris)
library(rgdal)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(googleVis)

# Read in the buildings, buses and pollutants data sets
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
  "Building benchmarking 2017.csv"
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

# Read in the borough shapefiles
boundaries = county_subdivisions("36", c("Bronx", "New York", "Kings", "Richmond", "Queens"), cb = FALSE)
boundaries = rmapshaper::ms_simplify(boundaries)

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

# Build separate data frames for use with leaflet, involving a change of coordinates for the bus data
nad83_coords = ungroup(buses %>% group_by(XCoordinates, YCoordinates) %>% summarise(count = n()))
coordinates(nad83_coords) = c('XCoordinates', 'YCoordinates')
proj4string(nad83_coords) = CRS("+init=esri:102718")
bus_by_garage = spTransform(nad83_coords, CRS("+init=epsg:4326"))
bus_by_garage = bus_by_garage[bus_by_garage$XCoordinates > -77,]

pollutants_by_group = pollutants %>% group_by(Latitude, Longitude) %>% summarise(count = n())

# Build another data set for plotting the vehicle pollutant measurements
vehicle_pollutants = pollutants %>% mutate(County.Name = ifelse(
  County.Name == "Richmond",
  "Staten Island",
  ifelse(
    County.Name == "Kings",
    "Brooklyn",
    ifelse(
      County.Name == "New York",
      "Manhattan",
      ifelse(County.Name == "Queens", "Queens", "Bronx")
    )
  )
)) %>%
  filter((
    substr(
      Parameter.Name,
      nchar(Parameter.Name) - 2,
      nchar(Parameter.Name)
    ) %in% c("ane", "ene", "yne")
  ) |
    (
      Parameter.Name %in% c(
        "Carbon monoxide",
        "Nitric oxide (NO)",
        "Nitrogen dioxide (NO2)",
        "Oxides of nitrogen (NOx)",
        "Reactive oxides of nitrogen (NOy)",
        "Sulfur dioxide"
      )
    ) |
    (
      Parameter.Name == "PM2.5 - Local Conditions" &
        Metric.Used == "Daily Mean" &
        Sample.Duration == "24 HOUR"
    )
  ) %>% group_by(Latitude,
                 Longitude,
                 Parameter.Name,
                 Units.of.Measure,
                 Sample.Duration,
                 County.Name) %>% summarise(observation = mean(Observation.Count))

