#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define a function for removing outliers from the emissions column of the data frame
remove_outliers = function(df) {
  df %>% filter((df$total_gg_MtCO2e < (2 * IQR(
    df$total_gg_MtCO2e
  ))) &
    (df$gross_sf < (2 * IQR(df$gross_sf))))
}


server <- function(input, output, session) {
  # Filter the buildings data according to the Shiny inputs
  buildings_out_filtered = reactive({
    if (input$outliers == 1) {
      remove_outliers(buildings_gVis %>% filter(borough %in% input$borough))
    } else {
      buildings_gVis %>% filter(borough %in% input$borough)
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
  
  # Make a reactive inforbox displaying the average GHG emissions of the buildings the user selects
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
  
  # Make a googlevis column chart showing the required changes in the city's emissions, by category
  # Data from https://www.dec.ny.gov/docs/administration_pdf/nyserdaghg2015.pdf
  # Buildings includes Residential, Commercial and Industrial FF combustion, and electricity generation
  output$changesPlot = renderGvis({
    gvisColumnChart(
      data.frame(
        year = c('1990','2015', '2030'),
        Buildings = c(143.7, 97.4, 58.4),
        Transportation = c(60.4, 72.8, 47.7),
        Waste = c(14.8, 13.2, 8.7),
        Industry = c(3.6, 12.2, 8.0),
        Agriculture = c(8.3, 8.9, 5.8)
      ),
      options = list(
        vAxis = "{title:'New York state GHG inventory MMtCO2e'}",
        hAxis = "{title:'Year'}",
        explorer = "{actions:['dragToZoom', 'rightClickToReset']}",
        legend = "{position: 'bottom', alignment: 'center'}",
        width = 600,
        height = 450,
        chartArea = "{left:60, top:25, width:'85%', height:'80%'}",
        isStacked = "true"
      )
    )
  })
  
  # Make an interactive googlevis scatter plot of buildings emissions vs size
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
  
  # Make an interactive ggplot histogram showing the distibutions of building emissions
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
  
  # Filter the vehice pollutants data according to the user's selected pollutant
  v_pollutants_filtered = reactive({
    if (input$pollutant == "All hydrocarbons") {
      vehicle_pollutants %>% filter(substr(
        Parameter.Name,
        nchar(Parameter.Name) - 2,
        nchar(Parameter.Name)
      ) %in% c("ane", "ene", "yne"))
    } else if (input$pollutant == "All nitrogen oxides") {
      vehicle_pollutants %>% filter(
        Parameter.Name %in% c(
          "Nitric oxide (NO)",
          "Nitrogen dioxide (NO2)",
          "Oxides of nitrogen (NOx)",
          "Reactive oxides of nitrogen (NOy)"
        )
      )
    } else {
      vehicle_pollutants %>% filter(Parameter.Name == input$pollutant)
    }
  })
  
  # Update the available sample durations according to the selected pollutant
  observe({
    updateSelectizeInput(session,
                         "duration",
                         choices = unique(v_pollutants_filtered()$Sample.Duration))
  })
  
  # Filter the vehice pollutants data again, this time according to the sample duration from those available
  # Then group by borough and join the pollutant level data to the borough shapefiles
  county_pollutants_joined = reactive({
    v_p_filt = v_pollutants_filtered() %>% filter(Sample.Duration == input$duration) %>% group_by(County.Name, Units.of.Measure, Sample.Duration) %>% summarise(Average = mean(observation))
    geo_join(boundaries, v_p_filt, "NAME", "County.Name")
  })
  
  # Make a table displaying the pollutant level data in the choropleth
  output$data = renderTable({
    county_pollutants_joined()[c('NAME', 'Average')]
  })
  
  
  # The palette used in the below map needs to adjust to the range of values in the selected pollutant levels
  pal = reactive({
    colorNumeric(c("#bcbddc", "#756bb1"), domain = county_pollutants_joined()[['Average']])
  })
  
  # Create a leaflet map that shows the sites of bus depos and pollutant measurement, as well as achoropleth showing the observed
  # levels of different pollutants in each borough
  output$busMap = renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey") %>% addTiles(group = "OSM") %>% addPolygons(
      data = county_pollutants_joined(),
      fillColor = ~ pal()(county_pollutants_joined()[['Average']]),
      fillOpacity = 0.4,
      opacity = 1,
      color = "#666",
      dashArray = "3",
      weight = 1.5,
      smoothFactor = 0.2,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7),
      group = "Pollutant levels"
    ) %>% addLegend(
      pal = pal(),
      values = county_pollutants_joined()[['Average']],
      position = 'topleft',
      title = paste0("Observation","<br>", "(", tolower(
        as.character(county_pollutants_joined()[['Units.of.Measure']][1])
      ), ")")
    ) %>% addCircleMarkers(
      lng = bus_by_garage$XCoordinates,
      lat = bus_by_garage$YCoordinates,
      radius = sqrt(bus_by_garage$count),
      fillOpacity = 0.6,
      stroke = FALSE,
      group = "Bus garages",
      color = "#FFD800"
    ) %>% addCircleMarkers(
      lng = pollutants_by_group$Longitude,
      lat = pollutants_by_group$Latitude,
      radius = sqrt(pollutants_by_group$count),
      fillOpacity = 0.6,
      stroke = FALSE,
      group = "Pollutant measurement sites",
      color = "#d95f0e"
    ) %>% addLayersControl(
      baseGroups = c("Grey", "OSM"),
      overlayGroups = c(
        "Bus garages",
        "Pollutant measurement sites",
        "Pollutant levels"
      ),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
}
