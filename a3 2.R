library(shiny)
library(leaflet)
library(geojsonio)
library(readxl)
library(rgdal)
library(rgeos)
library(plotly)
library(shinyjs)
library(ggiraph)
library(sf)
library(openxlsx)
library(dplyr)
library(tidyr)
library(shinyjs)

##################################
# Data Pre-Processing and Loading
##################################

# Load the GeoJSON data
lga_data <- st_read("data/merged_vic_lga.geojson")
lga_data <- st_transform(lga_data, 4326)
lga_data <- lga_data[, -(1:11)]

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')

# Load the birthrate geojson data
lga_data <- st_read("data/merged_vic_lga.geojson")
lga_data <- st_transform(lga_data, 4326)
lga_data <- lga_data[, -(1:11)]

#age and gender data 
age_sex_male_data = read.xlsx("data/Population estimates by age and sex, by LGA, 2001 to 2022.xlsx",sheet=2,startRow = 9)
age_sex_female_data = read.xlsx("data/Population estimates by age and sex, by LGA, 2001 to 2022.xlsx",sheet=3,startRow = 9)

# filter out only the Victoria's data
age_sex_male_data <- age_sex_male_data[age_sex_male_data$`S/T.name`=="Victoria",]
age_sex_female_data <- age_sex_female_data[age_sex_female_data$`S/T.name`=="Victoria",]

# Remove rows where any column contains "None"
age_sex_male_data <- age_sex_male_data %>%
  filter_all(all_vars(. != "None" & . != "NA"))

age_sex_female_data <- age_sex_female_data %>%
  filter_all(all_vars(. != "None" & . != "NA"))


# Load the merged data
merged_data <- readxl::read_excel("data/merged_victoria_income_geo_data.xlsx")
geo_data <- geojsonio::geojson_read("data/VIC LGA.geojson", what = "sp")
spdf <- SpatialPolygonsDataFrame(geo_data, merged_data)
centroids <- gCentroid(spdf, byid=TRUE)


##################
# USER INTERFACE #
##################

ui <- fluidPage(
  tags$script(src = "https://public.tableau.com/javascripts/api/tableau-2.min.js"),
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
    body {
      background-color: black;
      color: white;
      font-family: 'Yusei Magic', sans-serif;
    }
    
    /* Custom CSS for the selectInput sidebar */
    .selectize-control.single .selectize-input {
        background-color: #787878 !important; /* Background color */
        color: white !important; /* Text color */
        border: 1px solid #555 !important; /* Border */
        /* Add more custom styles as needed */
    }
      
    .selectize-control.single {
      background-color:  #787878; /* Background color */
      border: 1px solid #333; /* Border color */
      color: white; /* Text color */
      box-shadow: none; /* Remove shadow (if any) */
    }
    
    /* Custom CSS for the options dropdown */
    .selectize-dropdown {
      background-color:  #787878; /* Dropdown background color */
      border: 1px solid #333; /* Dropdown border color */
    }
    
    /* Custom CSS for the selected option */
    .selectize-dropdown-content .option {
      color: white; /* Option text color */
      background-color:  #787878;
    }
  
    /* Custom CSS for the selected option when hovered */
    .selectize-dropdown-content .option.hover {
      background-color:  #787878; /* Background color on hover */
      color: white; /* Option text color */
    }
    
    /* Custom CSS for the selected option's text and remove button */
    .selectize-input .item {
      background-color: #444; /* Selected option background color */
      color: white; /* Selected option text color */
    }
    
    #income-map-tab {
    background-color: #333; /* Background color */
    color: white; /* Text color */
    border: 1px solid #555; /* Border */
    /* Add more custom styles as needed */
    }
    
    /* Custom CSS for the Shiny navbar */
    .navbar {
      position: absolute;
      top: 10px;
      right: 10px;
      width: 200px;
      z-index: 1000;
      background-color: #333; /* Background color */
      color: #fff; /* Text color */
      border-bottom: 1px solid #555; /* Border at the bottom */
      border-radius: 5px; /* Add rounded corners */
    }
     /* Add a hover effect */
   .navbar:hover {
    background-color: #555;
    color: #fff;
    border-bottom: 1px solid #777;
   }

  /* Style links within the navbar */
  .navbar a {
    text-decoration: none;
    color: white;
    display: block;
    padding: 8px 0;
    transition: background-color 0.3s ease; /* Add transition for links */
  }

  /* Style links on hover */
  .navbar a:hover {
    background-color: #555;
    border-radius: 3px;
  }

  /* Add a close button or icon */
  .close-button {
   position: absolute;
   top: 10px;
   right: 10px;
   cursor: pointer;
   color: #fff;
   font-size: 20px;
  }

 /* Change the close button color on hover */
 .close-button:hover {
  color: #ff5733; /* Change to a different color on hover */
 }
    
    
  ")),
  navbarPage("",
             tabPanel("Income Map", id = "income-map-tab",
                      fluidPage(
                        #leafletOutput("map", height = "100vh"),
                        tags$div(
                          style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); z-index: 1000;",
                          selectInput("data_choice", "Select Data:", 
                                      choices = c("Mean Income" = "Mean $",
                                                  "Median Income" = "Median $",
                                                  "Gini Coefficient" = "Gini coefficient coef.",
                                                  "Number of Earners" = "Earners (persons)"))
                        ),
                        
                      )
             ),
             
       tabPanel("Population",
                fluidPage(
                  leafletOutput("population_map", height = "100vh"),
                  absolutePanel(top = 100, left = 40,
                                useShinyjs(),
                                sliderInput(
                                  "year", "Select Year:",
                                  min = 2011,   # Minimum year value
                                  max = 2021,   # Maximum year value
                                  value = 2011, # Initial value
                                  step = 1      # Step size (1 year)
                                ),
                                actionButton("start_stop", "Start")
                   ),
                  absolutePanel( top = 400,     # Position from the top of the page (in pixels)
                                 left = 40,    # Position from the left of the page (in pixels)
                                 width = 400,   # Width of the panel (in pixels)
                                 height = 400,  # Height of the panel (in pixels)
                                 
                                 # Render a plot within the absolute panel
                                 plotOutput("genderAgePlot")
                                )
                  )
                  
                )
       )
)

  

################
# SHINY SERVER #
################

server <- function(input, output,session) {
  
  ################ birth rate and gender age structure ########################
  current_year <- reactiveVal(2011)
  is_running <- reactiveVal(FALSE)
  
  auto_increment_timer <- reactiveTimer(3000)  # 5 seconds timer
  
  observe({
    if (is_running()) {
      auto_increment_timer()  # Trigger the timer
    }
  })
  
  observeEvent(auto_increment_timer(), {
    if (is_running() && current_year() < 2021) {
      current_year(current_year() + 1)
    } else {
      is_running(FALSE)
    }
  })
  
  observeEvent(input$year, {
    if (!is_running()) {
      current_year(input$year)
    }
  })
  
  observeEvent(input$start_stop, {
    if (!is_running() && input$start_stop > 0) {
      is_running(TRUE)
      updateActionButton(session, "start_stop", label = "Stop")
    } else {
      is_running(FALSE)
      updateActionButton(session, "start_stop", label = "Start")
    }
  })
  
  observe({
    updateSliderInput(session, "year", value = current_year())
  })
  
  output$population_map <- renderLeaflet({
    target_col <- paste0("X", input$year, "_persons")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = lga_data[[target_col]]
    )
    mel_data = lga_data %>%
       filter(LGA.Name == "MELBOURNE")
    
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        data = mel_data,
        fillColor = ~pal(get(target_col)),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        label = ~paste(LGA.Name, "<br>Population: ", get(target_col))
      ) %>%
      addLegend(
        pal = pal,
        values = lga_data[[target_col]],
        title = "Population",
        position = "bottomright"
      )%>%
      addLabelOnlyMarkers(
        data = mel_data,
        lat = ~-37.8136,  # Replace with the actual column name for latitude
        lng = ~144.9631,  # Replace with the actual column name for longitude
        label = ~as.character(get(target_col)),
        labelOptions = labelOptions(noHide = TRUE, style = list("font-size" = "28px",
                                                                "background-color" = "transparent", 
                                                                "color" = "black"))
      )
  })
  
  output$genderAgePlot <- renderPlot({
    # Using gsub to extract the numeric part
    numeric_part <- input$year
    #print(numeric_part)
    filtered_male <- age_sex_male_data %>%
      filter(Year == numeric_part, LGA.name == "Melbourne")
    filtered_female <- age_sex_female_data %>%
      filter(Year == numeric_part, LGA.name == "Melbourne")
    
    # Combine male and female data sets
    filtered_male$Gender <- "Male"
    filtered_female$Gender <- "Female"
    combined_data <- bind_rows(filtered_male, filtered_female)
    
    # Reshape the data into long format
    combined_data_long <- combined_data %>%
      pivot_longer(cols = -c('Year', 'S/T.code', 'S/T.name', 'LGA.code', 'LGA.name', 'Gender', 'Total.males', 'Total.females'),
                   names_to = "Age_Group", values_to = "Population")
    
    # Add a column for sorting Age_Group in ascending order
    combined_data_long <- combined_data_long %>%
      mutate(Age_Group_Sort = factor(Age_Group, levels = unique(Age_Group)))
    
    # Multiply female values by -1 to place them on both sides of male bars
    combined_data_long <- combined_data_long %>%
      mutate(Population = ifelse(Gender == "Female", -Population, Population))
    
    # Create the age-gender pyramid plot with female bars on both sides and sorted age groups
    ggplot(combined_data_long, aes(x = Age_Group_Sort, y = Population, fill = Gender)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_y_continuous(labels = abs, expand = c(0, 0)) +
      scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"), name = "") +
      coord_flip() +
      facet_wrap(. ~ Gender, scale = "free_x", strip.position = "bottom") +
      labs(title = "Age-Gender Pyramid",
           x = "Age Group",
           y = "Population",
           fill = "Gender") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.spacing.x = unit(0, "pt"),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA))
    
  })
#################################################################################
}

  


#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
