#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(leaflet)

remove_outliers = function(df) {
  df %>% filter((df$total_gg_MtCO2e < (2 * IQR(
    df$total_gg_MtCO2e
  ))) &
    (df$gross_sf < (2 * IQR(df$gross_sf))))
}

server <- function(input, output) {
  buildings_borough_filtered = reactive({
    buildings_gVis %>% filter(borough %in% input$borough)
  })
  
  buildings_out_filtered = reactive({
    if (input$outliers == 1) {
      remove_outliers(buildings_borough_filtered())
    } else {
      buildings_borough_filtered()
    }
  })
  
  buildings_year_filtered = reactive({
    buildings_out_filtered() %>% filter((year >= input$year[1]) &
                                          (year <= input$year[2]))
  })
  
  buildings_filtered = reactive({
    if (input$property_type == "All") {
      buildings_year_filtered()
    } else {
      buildings_year_filtered() %>% filter(property_type == input$property_type)
    }
  })
  
  output$av_build_box = renderInfoBox({
    av_value = paste(c(as.character(round(
      mean(buildings_filtered()[, "total_gg_MtCO2e"])
    )), "MtCO2e"),
    sep = "",
    collapse = " ")
    infoBox(
      "Average emissions of selected buildings",
      av_value,
      icon = icon("calculator"),
      color = 'green'
    )
  })
  
  output$changesPlot = renderGvis({
    gvisColumnChart(
      data.frame(
        year = c('2015', '2030'),
        Buildings = c(97.4, 58.4),
        Transportation = c(72.8, 47.7),
        Waste = c(13.2, 8.7),
        Industry = c(12.2, 8.0),
        Agriculture = c(8.9, 5.8)
      ),
      options = list(
        vAxis = "{title:'New York state GHG inventory MMtCO2e'}",
        hAxis = "{title:'Year'}",
        explorer = "{actions:['dragToZoom', 'rightClickToReset']}",
        legend = "{position: 'bottom', alignment: 'center'}",
        width = 600,
        height = 470,
        chartArea = "{left:60, top:25, width:'85%', height:'80%'}",
        isStacked = "true"
      )
    )
  })
  
  output$buildingsPlot = renderGvis({
    gvisScatterChart(
      buildings_filtered()[c(
        "gross_sf",
        "Bronx",
        "Bronx.html.tooltip",
        "Brooklyn",
        "Brooklyn.html.tooltip",
        "Manhattan",
        "Manhattan.html.tooltip",
        "Queens",
        "Queens.html.tooltip",
        "Staten Island",
        "Staten Island.html.tooltip"
      )],
      options = list(
        pointSize = 5,
        vAxis = "{title:'Total GHG emissions MtCO2e'}",
        hAxis = "{title:'Gross building square footage - thousands'}",
        explorer = "{actions:['dragToZoom', 'rightClickToReset']}",
        dataOpacity = 0.5,
        series = "[{color:'#1b9e77'}, {color:'#d95f02'}, {color:'#7570b3'}, {color:'#e7298a'}, {color:'#66a61e'}]",
        legend = "{position: 'top', alignment: 'center'}",
        width = 800,
        height = 450,
        chartArea = "{left:60, top:25, width:'100%', height:'85%'}"
      )
    )
  })
  
  output$buildingsHisto = renderPlot(
    ggplot(data = buildings_filtered(), aes(x = total_gg_MtCO2e)) + geom_density(aes(color = borough), size = 2) + theme(
      axis.ticks = element_line(colour = "gray80"),
      panel.grid.major = element_line(colour = "gray80"),
      panel.grid.minor = element_line(colour = "gray80"),
      axis.title = element_text(size = 15,
                                face = "italic"),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      panel.background = element_rect(fill = NA)
    ) + labs(x = "Total GHG emissions MtCO2e", y = "Density",
             colour = "Borough") + scale_color_brewer(palette = "Dark2") +
      xlim(0, 1200) + ylim(0, 0.0032)
  )
  
  vehicle_pollutants_filtered = reactive({
    if (input$pollutant == "All hydrocarbons") {
      vehicle_pollutants %>% filter(substr(Parameter.Name, nchar(Parameter.Name) - 2, nchar(Parameter.Name)) %in% c("ane", "ene", "yne"))
    } else if (input$pollutant == "All nitrogen oxides") {
      vehicle_pollutants %>% filter(Parameter.Name %in% c("Nitric oxide (NO)", "Nitrogen dioxide (NO2)", "Oxides of nitrogen (NOx)", "Reactive oxides of nitrogen (NOy)"))
    } else {
      vehicle_pollutants %>% filter(Paramter.name == input$pollutant)
    }
  })
  
  output$busMap = renderLeaflet({
    leaflet() %>% addPolygons(data = nyboroughs) %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% addCircleMarkers(
      lng = bus_by_garage$x[bus_by_garage$x > -77],
      lat = bus_by_garage$y[bus_by_garage$x > -77],
      radius = sqrt(bus_by_garage$count),
      fillOpacity = 0.5,
      group = "Bus Garages",
      color = "#FFD800"
    ) %>% addCircleMarkers(
      lng = pollutants_by_group$Longitude,
      lat = pollutants_by_group$Latitude,
      radius = sqrt(pollutants_by_group$count),
      fillOpacity = 0.5,
      group = "Pollutant Measurement"
    ) %>% addLayersControl(
      overlayGroups = c("Bus Garages", "Pollutant Measurement"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
}
