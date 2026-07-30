# Load required libraries
# Data source https://www.kaggle.com/datasets/sujaykapadnis/every-known-satellite-orbiting-earth

library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

# Reading dataset
satellite_data <- read.csv("UCS-Satellite-Database-1-1-2023.csv", header = TRUE, sep = ",")

# Data processing
# Convert launch date to proper the US Date format
satellite_data$Date.of.Launch <- as.Date(satellite_data$Date.of.Launch, format = "%d-%m-%Y")
satellite_data$Year <- format(satellite_data$Date.of.Launch, "%Y")
satellite_data$Year <- as.numeric(satellite_data$Year)

# Use Country of Operator/Owner for country data
satellite_data$Country.Simple <- sapply(strsplit(as.character(satellite_data$Country.of.Operator.Owner), "/"), function(x) x[1])
satellite_data$Country.Simple <- trimws(satellite_data$Country.Simple)

# Class of Orbit
satellite_data$Class.of.Orbit <- toupper(as.character(satellite_data$Class.of.Orbit))

# Get the map data once, at startup. The polygons never change, so building
# them again inside renderPlot made every filter change pay for it a second time.
world_map <- map_data("world")
world_map_regions <- unique(world_map$region)

# Cache for the rendered plots. This is created explicitly rather than relying
# on bindCache()'s default cache = "app": under webR (the WebAssembly build the
# GitHub Pages site runs on) that default is not configured, and bindCache
# fails with "`cache` must either be "app", "session", or a cache object".
plot_cache <- cachem::cache_mem(max_size = 100 * 1024^2)

# Create a country mapping dictionary to match exact names in the maps package
country_mapping <- list(
  "UK" = "UK",
  "United Kingdom" = "UK",
  "Britain" = "UK",
  "Great Britain" = "UK",
  "England" = "UK",
  "England/UK" = "UK",
  "USA" = "USA",
  "United States" = "USA",
  "US" = "USA",
  "U.S." = "USA",
  "U.S.A." = "USA",
  "Russia" = "Russia",
  "Russian Federation" = "Russia",
  "China" = "China",
  "People's Republic of China" = "China",
  "PRC" = "China",
  "Japan" = "Japan"
)

# Apply country mapping for better matching with world map
satellite_data$Country.Map <- satellite_data$Country.Simple

# Apply country mapping
for (country in names(country_mapping)) {
  satellite_data$Country.Map[satellite_data$Country.Map == country] <- country_mapping[[country]]
}

# Set multinational and non-registered to NA for mapping 
satellite_data$Country.Map[satellite_data$Country.Map %in% c("Multinational", "NR", "")] <- NA

# Get unique countries that actually have satellites in the dataset
countries_with_satellites <- satellite_data %>%
  filter(!is.na(Country.Simple) & Country.Simple != "NR" & Country.Simple != "") %>%
  group_by(Country.Simple) %>%
  summarise(Count = n()) %>%
  filter(Count > 0) %>%
  arrange(desc(Count)) %>%
  pull(Country.Simple)

# Get unique values for other filters
all_users <- unique(satellite_data$Users)
all_users <- all_users[!is.na(all_users) & all_users != ""]

all_purposes <- unique(satellite_data$Purpose)
all_purposes <- all_purposes[!is.na(all_purposes) & all_purposes != ""]

# Get unique orbit classes 
all_orbits <- unique(satellite_data$Class.of.Orbit)
all_orbits <- all_orbits[!is.na(all_orbits) & all_orbits != ""]
all_orbits <- sort(all_orbits) 

# Define UI 
ui <- fluidPage(
  titlePanel(
    div(
      "UCS Satellite Database Interactive Visualization",
      div(
        style = "float: right; font-size: 14px; margin-top: 5px;",
        tags$a(href = "User_Guide.docx", "Download User Guide", download = TRUE)
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      
      # Date inputs in American format (MM/DD/YYYY)
      dateInput("startDate",
                "Start Date:",
                value = min(satellite_data$Date.of.Launch, na.rm = TRUE),
                min = min(satellite_data$Date.of.Launch, na.rm = TRUE),
                max = max(satellite_data$Date.of.Launch, na.rm = TRUE),
                format = "mm/dd/yyyy"),  
      
      dateInput("endDate",
                "End Date:",
                value = max(satellite_data$Date.of.Launch, na.rm = TRUE),
                min = min(satellite_data$Date.of.Launch, na.rm = TRUE),
                max = max(satellite_data$Date.of.Launch, na.rm = TRUE),
                format = "mm/dd/yyyy"),  
      
      # Country selection 
      # Only showing countries with satellites 
      
      h5("Countries"),
      checkboxInput("selectAllCountries", "Select All Countries", TRUE),
      conditionalPanel(
        condition = "!input.selectAllCountries",
        selectInput("countries",
                    "Choose Countries (max 10 for the line plot):",
                    choices = countries_with_satellites,
                    multiple = TRUE,
                    selected = head(countries_with_satellites, 5))
      ),
      
      # User selection with radio buttons
      h5("User Type"),
      radioButtons("users",
                   "Choose User Type:",
                   choices = c("All" = "all", 
                               "Civil" = "Civil",
                               "Commercial" = "Commercial",
                               "Government" = "Government",
                               "Military" = "Military"),
                   selected = "all"),
      
      # Orbit class selection with Checkboxes 
      h5("Class of Orbit"),
      checkboxGroupInput("orbitClass",
                         "Choose Orbit Classes:",
                         choices = all_orbits,
                         selected = all_orbits),
      # Add error message for orbit class selection
      htmlOutput("orbitClassError"),
      
      # Purpose selection 
      h5("Purpose"),
      checkboxInput("selectAllPurposes", "Select All Purposes", TRUE),
      conditionalPanel(
        condition = "!input.selectAllPurposes",
        selectInput("purposes",
                    "Choose Purposes:",
                    choices = all_purposes,
                    multiple = TRUE,
                    selected = all_purposes[1:3])
      ),
      
      width = 3  
    ),
    
    mainPanel(
      fluidRow(
        column(12, 
               h3("A Map of Satellites Launched to Space"),
               # Add a conditional panel to show error or plot
               conditionalPanel(
                 condition = "output.hasValidSelection",
                 plotOutput("worldHeatmap", height = "350px")
               ),
               conditionalPanel(
                 condition = "!output.hasValidSelection",
                 div(
                   style = "height: 350px; display: flex; align-items: center; justify-content: center;",
                   h4("Please select at least one orbit class to display data", style = "color: red;")
                 )
               )
        )
      ),
      
      fluidRow(
        column(12, 
               h3("Number of Satellites Launched"),
               # Add a conditional panel to show error or plot
               conditionalPanel(
                 condition = "output.hasValidSelection",
                 plotOutput("timeSeries", height = "350px")
               ),
               conditionalPanel(
                 condition = "!output.hasValidSelection",
                 div(
                   style = "height: 350px; display: flex; align-items: center; justify-content: center;",
                   h4("Please select at least one orbit class to display data", style = "color: red;")
                 )
               )
        )
      ),
      
      # Notifying Users of the Date Range
      fluidRow(
        column(12, 
               hr(),
               p("This data spans from November 15 1973 to December 28 2022", style = "text-align: center; font-style: italic"),
               div(
                 style = "text-align: center; margin-top: 10px;",
                 tags$a(href = "User_Guide.docx", "Need help? Download the User Guide", download = TRUE)
               )
        )
      ),
      
      width = 9  
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create a reactive value to track validation status
  validSelection <- reactive({
    length(input$orbitClass) > 0
  })
  
  # Output the validation status for use in conditionalPanel
  output$hasValidSelection <- reactive({
    validSelection()
  })
  outputOptions(output, "hasValidSelection", suspendWhenHidden = FALSE)
  
  # Display error message for orbit class
  output$orbitClassError <- renderUI({
    if (!validSelection()) {
      div(
        style = "color: red; margin-top: 5px;",
        "Please select at least one orbit class"
      )
    }
  })
  
  # The whole filter state as one value, debounced so that a burst of changes
  # (ticking several orbit classes in a row, typing a date) settles into a
  # single re-render instead of one per click.
  #
  # This is deliberately also the cache key for both plots below. Keying the
  # cache on the raw inputs instead would break the app: the key would change
  # the instant an input did, so a render would run against the not-yet-updated
  # debounced data and store that stale image under the new key. The debounced
  # value would then arrive without changing the key, and the stale image would
  # be served from cache forever.
  filter_state <- reactive({
    list(startDate = input$startDate,
         endDate = input$endDate,
         selectAllCountries = input$selectAllCountries,
         countries = input$countries,
         users = input$users,
         orbitClass = input$orbitClass,
         selectAllPurposes = input$selectAllPurposes,
         purposes = input$purposes)
  }) %>% debounce(300)

  # Reactive function to filter data based on inputs
  filtered_data <- reactive({
    f <- filter_state()

    # Return NULL if validation fails
    if (length(f$orbitClass) == 0) {
      return(NULL)
    }

    data <- satellite_data

    # Filter by date
    data <- data %>%
      filter(Date.of.Launch >= f$startDate & Date.of.Launch <= f$endDate)

    # Filter by countries
    if (!f$selectAllCountries) {
      data <- data %>%
        filter(Country.Simple %in% f$countries)
    }

    # Filter by users using radio buttons
    if (f$users != "all") {
      data <- data %>%
        filter(Users == f$users)
    }

    # Filter by orbit class using checkboxes
    data <- data %>%
      filter(Class.of.Orbit %in% f$orbitClass)

    # Filter by purposes
    if (!f$selectAllPurposes) {
      data <- data %>%
        filter(Purpose %in% f$purposes)
    }

    data
  })

  # World heatmap
  output$worldHeatmap <- renderPlot({
    # Check if we have valid data
    req(filtered_data())

    # Calculate satellite counts by country (using operator/owner)
    country_counts <- filtered_data() %>%
      group_by(Country.Map) %>%
      summarise(Count = n(), .groups = "drop") %>%
      filter(!is.na(Country.Map))
    
    # Join with world map data
    world_map_data <- world_map %>%
      left_join(country_counts, by = c("region" = "Country.Map"))
    
    # Create heatmap with simplified style for better scaling
    ggplot(world_map_data, aes(x = long, y = lat, group = group, fill = Count)) +
      geom_polygon(color = "white", size = 0.1) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray",
                          name = "Number of\nSatellites") +
      theme_bw() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.margin = margin(0, 0, 0, 0))
  }) %>% bindCache(filter_state(), cache = plot_cache)

  # Line graph showing superimposed graph for selected countries
  output$timeSeries <- renderPlot({
    # Check if we have valid data
    req(filtered_data())
    
    # Read the same debounced state the cache key uses, so the rendered image
    # always matches the key it gets stored under.
    f <- filter_state()

    # Get selected countries or top countries if all selected
    if (f$selectAllCountries) {
      # Get top 8 countries by total satellites
      top_countries <- filtered_data() %>%
        group_by(Country.Simple) %>%
        summarise(Total = n(), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        head(8) %>%
        pull(Country.Simple)

      time_data <- filtered_data() %>%
        filter(Country.Simple %in% top_countries)
    } else {
      # Limit to max 10 countries for readability
      selected_countries <- head(f$countries, 10)
      time_data <- filtered_data() %>%
        filter(Country.Simple %in% selected_countries)
    }
    
    # Aggregate by year and country
    time_data <- time_data %>%
      group_by(Year, Country.Simple) %>%
      summarise(Count = n(), .groups = "drop") %>%
      filter(!is.na(Year) & !is.na(Country.Simple))

    # A filter combination can legitimately match nothing. Without this, the
    # seq() in scale_x_continuous below gets min/max of an empty vector
    # (Inf/-Inf) and the plot errors out instead of just showing nothing.
    req(nrow(time_data) > 0)

    # Create superimposed line graph
    ggplot(time_data, aes(x = Year, y = Count, color = Country.Simple, group = Country.Simple)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(x = "Year",
           y = "Number of Satellites Launched",
           color = "Country") +
      theme_bw() +
      scale_x_continuous(breaks = seq(min(time_data$Year, na.rm = TRUE), 
                                      max(time_data$Year, na.rm = TRUE), 
                                      by = 5)) +
      theme(legend.position = "right",
            plot.margin = margin(0, 0, 0, 0))
  }) %>% bindCache(filter_state(), cache = plot_cache)
}

# Run the application
shinyApp(ui = ui, server = server)
# We did it lads