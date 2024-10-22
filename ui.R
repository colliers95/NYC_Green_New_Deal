#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Build the available choices for building type, and assign a value to them, including an "All" option
propertyTypechoices = list("All" = "All")
for (pt in unique(buildings$property_type)) {
  propertyTypechoices[[as.character(pt)]] = as.character(pt)
}

header <-
  dashboardHeader(title = 'New York City\'s Green New Deal', titleWidth = 340)

sidebar <-
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Overview",
      tabName = "overview",
      icon = icon("sticky-note")
    ),
    menuItem("Buildings",
             tabName = "buildings",
             icon = icon("building")),
    menuItem("Buses",
             tabName = "buses",
             icon = icon("bus"))
  ))


body <-
  dashboardBody(tabItems(
    tabItem(
      # Info boxes for top of the overview page
      tabName = "overview",
      fluidRow(
        infoBox(
          "Number of bills in the act",
          10,
          icon = icon("newspaper"),
          width = 4,
          color = 'green'
        ),
        infoBox(
          "Margin by which the act passed",
          "45 to 2",
          icon = icon("check"),
          width = 4,
          color = 'green'
        ),
        infoBox(
          "Emissions reduction equivalent",
          "1 million cars",
          icon = icon("car"),
          width = 4,
          color = 'green'
        )
      ),fluidRow(box(tags$h4("New York State is aiming for a 40% reduction in greenhouse gas emissions from 1990 levels and for 50% of its energy to come from renewable energy sources by 2030"), width = 12)),
      fluidRow(
        box(
          # Description of the CLimate Mobilization Act, plot of require emissions changes and De Blasio youtube video
          title = "The act",
          "The Climate Mobilization Act passed city council on Thursday, 18th of April 2019. It aims to keep the city in line with emission reduction targets set by the international Paris climate agreement.",
          br(),
          br(),
          "It consists of five main elements:",
          br(),
          tags$ol(
            br(),
            tags$li(
              "A bill requiring feasibility studies to be conducted every 4 years, looking at how to replace the city's 24 fossil fuel power plants with renewable sources and energy storage."
            ),
            br(),
            tags$li(
              "Legislation requiring buildings over 25,000 square feet to cut emissions by 40% by 2030, and 80% by 2050, by retrofitting new windows and insulation. There will be fines for failing to meet targets."
            ),
            br(),
            tags$li(
              "A (yet to be voted on) measure to convert all school buses to electric within 20 years, part of New York City's goal to switch all public buses to electric by 2040."
            ),
            br(),
            tags$li(
              "Two bills in the package combine to stipulate that roofs of new and smaller buildings should be covered in plants, solar panels, mini wind turbines or some combination of the three."
            ),
            br(),
            tags$li(
              "A resolution to deny the relevant permit for the Williams pipeline, proposed to bring fracked natural gas from Pennsylvania to New York. Fracking is already banned in New York."
            )
          ),
          solidHeader = TRUE,
          height = "515px"
        ),
        box(
          title = "Required emissions changes", htmlOutput("changesPlot")
        ),
        fluidRow(column(12, offset = 1,
          tags$iframe(
            src = "https://www.youtube.com/embed/3zPzjwU2Nlo",
            width = "1075",
            height = "604.6875"
          )
        ))
      )),
      
      tabItem(
        # Buildings scatter plot and histogram, input selectors and average greenhouse gas emissions box
        tabName = "buildings",
        fluidRow(
          valueBox(
            "Dirty Buildings Bill",
            "Requires buildings over 25,000 sqft to cut emissions by 40 percent by 2030 and 80 percent by 2050",
            icon = icon("building"),
            width = 8,
            color = 'green'
          ),
          valueBoxOutput("av_build_box")
        ),
        fluidRow(
          tabBox(
            title = "Building analysis",
            width = 8,
            tabPanel("Scatter", htmlOutput("buildingsPlot")),
            tabPanel("Histogram", plotOutput("buildingsHisto"))
          ),
          box(
            title = "Options",
            width = 4,
            radioButtons(
              "outliers",
              label = "Include outliers?",
              choices = list("Keep outliers" = 0, "Remove outliers" = 1),
              selected = 1
            ),
            checkboxGroupInput(
              "borough",
              label = "Select boroughs",
              choices = list(
                "The Bronx" = "Bronx",
                "Brooklyn" = "Brooklyn",
                "Manhattan" = "Manhattan",
                "Queens" = "Queens",
                "Staten Island" = "Staten Island"
              ),
              selected = c("Manhattan")
            ),
            selectInput(
              "property_type",
              label = "Select a property category",
              choices = propertyTypechoices,
              selected = "All"
            ),
            sliderInput(
              "year",
              label = "Filter year built",
              min = 1827,
              max = 2019,
              value = c(1827, 2019)
            )
          )
        )
      ),
      
      tabItem(
        # Map of bus garages and pollutant information with inputs and description
        tabName = "buses",
              fluidRow(
                valueBox(
                  "School Bus Bill",
                  "Fulfilling the goal of making all of New Yorks buses fully electric in the next 20 years",
                  icon = icon("bus"),
                  width = 6,
                  color = 'green'
                ),
                tabBox(
                  title = "Description", 
                  tabPanel("Map Basics", "The chart below shows a pollutant choropleth and also maps current major bus garages and the locations of pollution measurement sites. The radius of these is proportional to the number of buses housed, and pollutants measured, respectively."),
                  tabPanel("Pollutants", "The pollutants included here are those most attributed to vehicle tailpipes. These include Sulfur Dioxide, Carbon Monoxide, Hydrocarbons, Nitrogen Oxides and small particulate matter.")
              )),
              fluidRow(
                box(
                  leafletOutput("busMap", width = "1025px", height = "650px"), 
                  tags$em("Hold shift while drawing a rectangle for box zoom"),
                  width = 10
                  ),
                box(
                  selectizeInput(
                    'pollutant', 'Select pollutant(s) to display', choices = c(c("All hydrocarbons", "All nitrogen oxides"), sort(vehicle_pollutants$Parameter.Name))
                  ), selectizeInput("duration", "Select sample duration", choices = unique(vehicle_pollutants$Sample.Duration)), tableOutput("data"), width = 2
                )
                )
              )
    ),
    
    # CSS for styling the dashboard title and overview description
    tags$head(tags$style(
      HTML(
        '
        .skin-green .main-header .logo {
        background-color: #00a65a;
        }
        .skin-green .main-header .logo:hover {
        background-color: #00a65a;
        }
        .skin-green .box-body {
        font-size: 14.7px;
        }
        '
      )
      )))
    
    dashboardPage(header, sidebar, body, skin = 'green')
    