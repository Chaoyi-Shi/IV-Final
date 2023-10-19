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
library(reshape2)
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

### Salary and jobs data
industry_data <- read.csv("Paul data/Number of jobs by Industry 2019.csv", header = TRUE)
industry_data["Area"] = industry_data["X.Area"] 
industry_data$Area <- gsub("^X\\.", "", industry_data$Area)
jobs_salary_data <- read.csv("Paul data/Number of Jobs and salary.csv", stringsAsFactors = FALSE)

total_data <- read.csv("Paul data/Number of Jobs and salary.csv", header = TRUE)
salary_data <- read.csv("Paul data/Income By Gender and area.csv", header = TRUE)

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
                        leafletOutput("map", height = "100vh"),
                        tags$div(
                          style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); z-index: 1000;",
                          selectInput("data_choice", "Select Data:", 
                                      choices = c("Mean Income" = "Mean $",
                                                  "Median Income" = "Median $",
                                                  "Gini Coefficient" = "Gini coefficient coef.",
                                                  "Number of Earners" = "Earners (persons)"))
                        ),
                        tags$div(
                          style = "position: absolute; left: 10px; top: 50%; transform: translateY(-50%); z-index: 1000; width: 400px; height: 500px;",
                          plotlyOutput("barplot")
                        )
                      )
             ),
             tabPanel("Building approved by State",
                      div(id = "tableauVizContainer", style = "height:500px;"),
                      uiOutput("embedTableauViz")
             ),
             tabPanel("Road infrastructure development",
                      div(id = "tableauVizRoad", style = "height:500px;"),
                      uiOutput("embedTableauVizRoad")
             ),
       tabPanel("Population",
                fluidPage(
                  leafletOutput("birth_rate_map", height = "100vh"),
                  absolutePanel(top = 100, left = 40,
                                selectInput(
                                            "year", "Select Year:", 
                                            choices=c('2011'= 'X2011_rate',
                                                      '2012'= 'X2012_rate',
                                                      '2013'= 'X2013_rate',
                                                      '2014'= 'X2014_rate',
                                                      '2015'= 'X2015_rate',
                                                      '2016'= 'X2016_rate',
                                                      '2017'= 'X2017_rate',
                                                      '2018'= 'X2018_rate',
                                                      '2019'= 'X2019_rate',
                                                      '2020'= 'X2020_rate',
                                                      '2021'= 'X2021_rate'),
                                            selected= 'X2021_rate'),
                                # LGA selection dropdown
                                selectInput("lga", "Select LGA:", 
                                            choices = unique(age_sex_male_data$LGA.name), 
                                            selected = unique(age_sex_male_data$LGA.name)[1])
                   ),
                  tags$script('
                  $(document).ready(function() {
                  // Make the absolute panel draggable
                  $("#draggablePanel").draggable({
                  containment: "parent" // Restrict movement to the parent container
                   });
                  '),
                  absolutePanel( top = 300,     # Position from the top of the page (in pixels)
                                 left = 40,    # Position from the left of the page (in pixels)
                                 width = 400,   # Width of the panel (in pixels)
                                 height = 400,  # Height of the panel (in pixels)
                                 
                                 # Render a plot within the absolute panel
                                 plotOutput("genderAgePlot")
                                )
                  )
                  
                ),
       tabPanel("Jobs and salary in Victoria by areas in 2019",
                tags$div("LGA: Local Government Area", style = "position: absolute; top: 60px; left: 10px; color: white; font-size: 24px; z-index: 100;"),  # Add this line
                tags$div("More Specific Area: Statistic Area Level 2", style = "position: absolute; top: 90px; left: 10px; color: white; font-size: 24px; z-index: 100;"),  # Add this line
                # Sidebar
                div(style = "float: left; width: 25%;",  # This CSS will make the sidebar float to the left and occupy 25% width
                    div(
                      style = "margin-top: 300px;",   # Adjust this value to position the bar chart as per your preference
                      selectInput("selected_area", "More Specific Area:", choices = sort(unique(industry_data$Area))),
                      girafeOutput("barChart"),
                      girafeOutput("lineChart")
                    )
                ),
                
                # Main charts
                div(style = "float: left; width: 75%;",  # This CSS will make the main content float to the left and occupy 75% width
                    div(
                      style = "margin-left: 350px;",
                      style = "margin-bottom: 10px;",  # Style adjustments for the slider
                      sliderInput("lgaRange", 
                                  "LGA Rank Range:", 
                                  min = 1, 
                                  max = length(unique(jobs_salary_data$Lga)), 
                                  value = c(1, 10),
                                  step = 1)
                    ),
                    div(
                      girafeOutput("rankedLgaChart"),
                      style = "margin-bottom: 20px;"   # Adds a margin to the bottom of the first chart
                    ),
                    div(
                      style = "margin-left: 400px;",   # Adjust this value to position the selectInput as per your preference
                      selectInput("selected_LGA", label = NULL, choices = sort(unique(jobs_salary_data$Lga)))
                    ),
                    girafeOutput("interactiveBarChart")
                ),
                
                # Clear float to ensure proper alignment of subsequent elements
                div(style = "clear: both;"))
       )
)

  

################
# SHINY SERVER #
################

server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create a color palette based on user's choice
    pal <- colorNumeric("Blues", domain = na.omit(spdf[[input$data_choice]]))
    
    leaflet(data = spdf) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(spdf[[input$data_choice]]), "gray", pal(spdf[[input$data_choice]])), 
        fillOpacity = 0.7,  
        color = "white",
        weight = 1,
        popup = paste0("<strong>LGA: </strong>", spdf$`LGA NAME`, 
                       "<br><strong>", input$data_choice, ": </strong>", spdf[[input$data_choice]])
      ) %>%
      addLegend(pal = pal, values = ~spdf[[input$data_choice]], title = input$data_choice, position = "bottomright") 
  })
  
  output$barplot <- renderPlotly({
    top1 <- merged_data$`Top 1% %`
    top5 <- merged_data$`Top 5% %`
    top10 <- merged_data$`Top 10% %`
    lga_names <- merged_data$`LGA NAME`
    
    data <- data.frame(lga_names, top1, top5, top10)
    
    plot_ly(data, x = ~lga_names, y = ~top1, type = 'bar', name = 'Top 1%') %>%
      add_trace(y = ~top5, name = 'Top 5%') %>%
      add_trace(y = ~top10, name = 'Top 10%') %>%
      layout(yaxis = list(title = 'Income Percentage'), barmode = 'group')
  })
  
  output$embedTableauViz <- renderUI({
    # 在此处指定 Tableau viz 的 URL
    viz_url <- "https://public.tableau.com/views/Book1_16968176228800/Dashboard3?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link"
    
    # 使用 JavaScript Embedding API 的 initViz 方法嵌入 Tableau viz
    script <- sprintf('
    <script>
      function initViz() {
        var containerDiv = document.getElementById("tableauVizContainer");
        var vizUrl = "%s";
        var options = {
          hideTabs: true, // 隐藏 Tableau 选项卡
          width: "100%%",  // 设置宽度
          height: "800px"  // 设置高度
        };
        
        var viz = new tableau.Viz(containerDiv, vizUrl, options);
      }
      initViz();
    </script>
  ', viz_url)
    
    # 返回 script
    HTML(script)
  })
  
  output$embedTableauVizRoad <- renderUI({
    # 在此处指定 Tableau viz 的 URL
    viz_url <- "https://public.tableau.com/views/Book1_16968176228800/RoadProject?:language=en-GB&:display_count=n&:origin=viz_share_link"
    
    # 使用 JavaScript Embedding API 的 initViz 方法嵌入 Tableau viz
    script <- sprintf('
    <script>
      function initViz() {
        var containerDiv = document.getElementById("tableauVizRoad");
        var vizUrl = "%s";
        var options = {
          hideTabs: true, // 隐藏 Tableau 选项卡
          width: "100%%",  // 设置宽度
          height: "800px"  // 设置高度
        };
        
        var viz = new tableau.Viz(containerDiv, vizUrl, options);
      }
      initViz();
    </script>
  ', viz_url)
    
    # 返回 script
    HTML(script)
  })
  
  ################ birth rate and gender age structure ########################
  output$birth_rate_map <- renderLeaflet({
    target_col <- input$year
    pal <- colorNumeric(
      palette = "Blues",
      domain = lga_data[[target_col]]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        data = lga_data,
        fillColor = ~pal(get(target_col)),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        label = ~paste(LGA.Name, "<br>Birth Rate: ", round(get(target_col), 2))
      ) %>%
      setView(lng = 145, lat = -38, zoom = 6.1)%>%
      addLegend(
        pal = pal,
        values = lga_data[[target_col]],
        title = "Birth Rate",
        position = "bottomright"
      )
  })
  
  output$genderAgePlot <- renderPlot({
    # Using gsub to extract the numeric part
    numeric_part <- gsub("[^0-9]", "", input$year)
    #print(numeric_part)
    filtered_male <- age_sex_male_data %>%
      filter(Year == numeric_part, LGA.name == input$lga)
    filtered_female <- age_sex_female_data %>%
      filter(Year == numeric_part, LGA.name == input$lga)
    
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
            strip.background = element_rect(colour = "black"))
    
  })
#################################################################################
  library(RColorBrewer)
  
  output$barChart <- renderGirafe({
    selected_data <- industry_data[industry_data$Area == input$selected_area, ]
    selected_data <- na.omit(selected_data)  # Remove rows with NA values
    long_data <- melt(selected_data, id.vars="Area")
    
    # Filter out Lga variable
    long_data <- long_data[!long_data$variable %in% c("X.Area", "Lga", "Total"), ]
    
    # Create the tooltip_info column with same format as x-axis
    long_data$tooltip_info <- paste("Industry:", gsub("\\.", " ", long_data$variable), "<br>Jobs:", long_data$value)
    
    p <- ggplot(long_data, aes(x = "", y = value, fill = variable, tooltip = tooltip_info, data_id = variable)) +  
      geom_bar_interactive(stat = "identity", width = 0.6, position = "stack", alpha = 0.7) +  
      coord_polar(theta = "y") + 
      labs(title = paste("Jobs by Industry for", input$selected_area, "in 2019"), 
           x = NULL, y = NULL) +
      theme_void() +
      theme(
        legend.position = "right",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white")
      ) +
      scale_fill_viridis_d()  # 使用与径型柱状图相同的颜色调色板
    
    girafe(ggobj = p, width = 9, height = 6.4, 
           options = list(
             tooltip_offy = -50,  # Adjust tooltip position to appear in the hole
             tooltip_offx = 0,
             hover_opacity = 0.7,  # Highlight the bar when hovered
             onclick = "function(id){ alert('You clicked on: ' + id); }"  # Display an alert with the clicked bar's id
           ))
  })
  
  
  
  output$interactiveBarChart <- renderGirafe({
    # Filter data based on selected LGA
    filtered_data <- subset(jobs_salary_data, Lga == input$selected_LGA)
    
    # Add a tooltip column
    filtered_data$tooltip_info <- paste("Area:", filtered_data$Area, 
                                        "<br>Jobs:", as.numeric(gsub(",", "", filtered_data$X2019.20)))
    
    # Plotting
    p <- ggplot(filtered_data, aes(x = Area, y = as.numeric(gsub(",", "", `X2019.20`)), 
                                   tooltip = tooltip_info, data_id = Area, fill = Area)) +
      geom_bar_interactive(stat = "identity") +
      coord_polar(start = 0) +  # Convert the bar chart to a radial bar chart
      scale_fill_viridis_d() +  # Apply a color palette
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold", color = "white"),
        axis.ticks.y = element_blank(),  # Remove radial Y-axis ticks
        axis.text.y = element_blank(),  # Remove radial Y-axis text
        panel.grid.major = element_line(color = "grey80"),  # Add light grey radial grid lines
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        plot.title = element_text(color = "white"),
        legend.position = "none"  # Hide legend
        
      ) +
      labs(y = NULL, x = NULL, title = paste("Specific Area Average Number of Jobs in", input$selected_LGA, "for 2019-20"))
    
    girafe(ggobj = p,  width = 9, height = 9.3)
  })
  
  
  
  output$lineChart <- renderGirafe({
    # Prepare total_data for plotting
    selected_total_data <- total_data[total_data$Area == input$selected_area, ]
    long_total_data <- melt(selected_total_data, id.vars = "Area")
    long_total_data$type <- "Jobs"
    
    # Filter out Lga variable
    long_total_data <- long_total_data[long_total_data$variable != "Lga", ]
    
    # Prepare salary_data for plotting
    selected_salary_data <- salary_data[salary_data$Area == input$selected_area, ]
    long_salary_data <- melt(selected_salary_data, id.vars = "Area")
    long_salary_data$type <- "Salary"
    
    # Filter out Lga variable
    long_salary_data <- long_salary_data[long_salary_data$variable != "Lga", ]
    combined_data <- rbind(long_total_data, long_salary_data)
    combined_data$value <- gsub(",", "", combined_data$value)
    combined_data$value <- as.numeric(combined_data$value)
    
    selected_combined_data <- combined_data[combined_data$Area == input$selected_area, ]
    # Extract year from the variable and remove non-year characters
    combined_data$Year <- as.numeric(gsub("^X(\\d{4}).*", "\\1", combined_data$variable))
    p_total <- ggplot(combined_data, aes(x = Year, y = value, group = interaction(Area, type), color = type)) +
      geom_line(aes(group = interaction(Area, type))) + # Use geom_line instead of geom_line_interactive to make it not clickable
      geom_text(aes(label = value), vjust = -0.5) + # Add labels to each data point
      labs(title = paste("Average Jobs and Salary in", input$selected_area, "from 2015 to 2019"),
           x = "Year",
           y = "Average salary and number of jobs") +
      theme(
        panel.grid.major = element_blank(),  # Remove major grid
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        plot.title = element_text(color = "white")
      )+
      scale_color_manual(values = c("Jobs" = "blue", "Salary" = "red"))+
      scale_x_continuous(breaks = unique(combined_data$Year))
    
    
    girafe(ggobj = p_total, width = 9, height = 7)
  })
  output$rankedLgaChart <- renderGirafe({
    # Group by LGA and sum the number of jobs
    aggregated_data <- jobs_salary_data %>%
      group_by(Lga) %>%
      summarise(TotalJobs = sum(as.numeric(gsub(",", "", `X2019.20`)), na.rm = TRUE)) %>%
      arrange(-TotalJobs)
    
    # Filter based on the LGA rank range
    aggregated_data <- aggregated_data[input$lgaRange[1]:input$lgaRange[2], ]
    
    # Adding tooltip to show number of jobs
    aggregated_data$tooltip_text <- paste(aggregated_data$Lga, ": ", aggregated_data$TotalJobs, " jobs")
    
    p <- ggplot(aggregated_data, aes(x = reorder(Lga, -TotalJobs), 
                                     y = TotalJobs,
                                     tooltip = tooltip_text, data_id = Lga)) + 
      geom_bar_interactive(stat = "identity", fill = "orange") +  # Set the bar color to dark blue
      coord_flip() +
      labs(y = "Total Number of Jobs", x = "LGA", title = "Number of Jobs in Selected LGAs in 2019-20") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        panel.border = element_rect(fill=NA, color="black")  # Add border
      )
    
    girafe(ggobj = p, width = 8, height = 7)
  })
  
  
}

  


#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
