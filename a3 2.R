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
       tabPanel(tags$div("LGA: Local Government Area"),  # Add this line
                tags$div("More Specific Area: Statistic Area Level 2"),  # Add this line
                tags$style(HTML("
    body {
      background-color: black;
      margin: 0 !important;
      padding: 0 !important;
    }
    .shiny-output-error {
      display: none;
    }
    #yearFilterDiv {
      margin-bottom: 10px;
      background-color: #333;
      padding: 5px;
      border-radius: 5px;
    }
    #sideBarPanel {
      background-color: #222;
      padding: 10px;
      border-radius: 5px;
    }
  ")),
                titlePanel("Employment and salary condition in City of Melbourne"),
                sidebarLayout(
                  sidebarPanel(
                    id = "sideBarPanel",
                    selectInput("selected_area", "More Specific Area:", choices = sort(unique(industry_data$Area[industry_data$Lga == "Melbourne"]))),
                    tags$div(class="custom-girafe-output", girafeOutput("barChart")),
                    tags$div(class="custom-girafe-output", girafeOutput("lineChart")),
                    girafeOutput("lineChartMelbourneAverage")
                  ),
                  mainPanel(
                    tags$div(id = "yearFilterDiv", 
                             selectInput(inputId = "selectedYear", label = "Select Year", 
                                         choices = c("2016.17" = "X2016.17", 
                                                     "2017.18" = "X2017.18", 
                                                     "2018.19" = "X2018.19", 
                                                     "2019.20" = "X2019.20"),
                                         selected = "X2019.20")
                    ),
                    girafeOutput("rankedLgaChart"),
                    girafeOutput("interactiveBarChart")
                  ))
                )
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
  
  set3_colors <- brewer.pal(12, "Set3")
  pastel1_colors <- brewer.pal(9, "Pastel1")
  pastel2_colors <- brewer.pal(8, "Pastel2")
  combined_palette <- c(set3_colors, pastel1_colors, pastel2_colors[1:7])  # We need 7 colors from Pastel2 to get a total of 19
  
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
      labs(title = paste("Number of Jobs in different Industry in 2019"), 
           x = NULL, y = NULL) +
      theme_void() +
      theme(
        legend.position = "left",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white", size = 25),
        plot.title = element_text(color = "white",size= 25)
      ) +
      scale_fill_manual(values = combined_palette)
    
    girafe(ggobj = p, width = 16, height = 9, 
           options = list(
             tooltip_offy = -50,  # Adjust tooltip position to appear in the hole
             tooltip_offx = 0,
             hover_opacity = 0.7,  # Highlight the bar when hovered
             onclick = "function(id){ alert('You clicked on: ' + id); }"  # Display an alert with the clicked bar's id
           ))
  })
  
  
  
  output$interactiveBarChart <- renderGirafe({
    # Obtain the selected year from the input
    selected_column <- input$selectedYear
    
    # Filter data
    filtered_data <- subset(jobs_salary_data, Lga == "Melbourne")
    
    # Ensure the selected_column exists in the dataframe
    if (!selected_column %in% names(filtered_data)) {
      return(NULL)
    }
    
    # Add a tooltip column
    filtered_data$tooltip_info <- paste("Area:", filtered_data$Area, 
                                        "<br>Jobs:", as.numeric(gsub(",", "", filtered_data[[selected_column]])))
    
    # Plotting
    p <- ggplot(filtered_data, aes(x = Area, y = as.numeric(gsub(",", "", filtered_data[[selected_column]])), 
                                   tooltip = tooltip_info, data_id = Area, fill = Area)) +
      geom_bar_interactive(stat = "identity") +
      coord_polar(start = 0) +
      scale_fill_viridis_d() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold", color = "white"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.border = element_blank(),
        plot.title = element_text(color = "white", size = 30),
        legend.position = "none"
      ) +
      labs(y = NULL, x = NULL, title = ("Specific Area Average Number of Jobs in Melbourne"))
    
    girafe(ggobj = p, width = 15.0, height = 15.51)
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
      geom_line(aes(group = interaction(Area, type))) + 
      geom_text(aes(label = value), vjust = -0.5,size = 6) + 
      labs(title = paste("Average Jobs and Salary in", input$selected_area, "from 2015 to 2019"),
           x = NULL,
           y = "Average salary and number of jobs") +
      theme(
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text.x = element_text(color = "white", size = 14),  # Adjusting x-axis text
        axis.text.y = element_text(color = "white", size = 14),  # Adjusting y-axis text
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        plot.title = element_text(color = "white",size = 20)
      ) +
      scale_color_manual(values = c("Jobs" = "lightblue", "Salary" = "red")) +
      scale_x_continuous(breaks = unique(combined_data$Year))
    
    
    girafe(ggobj = p_total, width = 9, height = 7)
  })
  selected_lgas <- c("Banyule", "Bayside", "Boroondara", "Darebin", "Glen Eira", 
                     "Maribyrnong", "Monash", "Melbourne", "Moonee Valley", "Moreland",
                     "Port Phillip", "Stonnington", "Whitehorse", "Yarra")
  
  output$rankedLgaChart <- renderGirafe({
    # Group by LGA and sum the number of jobs
    selected_column <- input$selectedYear
    
    # Group by LGA and sum the number of jobs
    aggregated_data <- jobs_salary_data %>%
      filter(Lga %in% selected_lgas) %>%
      group_by(Lga) %>%
      summarise(TotalJobs = sum(as.numeric(gsub(",", "", !!sym(selected_column)))))
    
    # Adding tooltip to show number of jobs
    aggregated_data$tooltip_text <- paste(aggregated_data$Lga, ": ", aggregated_data$TotalJobs, " jobs")
    
    p <- ggplot(aggregated_data, aes(x = reorder(Lga, -TotalJobs), 
                                     y = TotalJobs, tooltip = tooltip_text, data_id = Lga)) + 
      geom_bar_interactive(stat = "identity", aes(fill = ifelse(Lga == "Melbourne", "Melbourne", "Others"))) +  # Conditional fill with tooltips
      scale_fill_manual(values = c("Melbourne" = "blue", "Others" = "orange")) +  # Manual color assignment for Melbourne
      coord_flip() +
      labs(y = "Total Number of Jobs", x = "LGA", title = "Average Number of Jobs in Melbourne and surrounding LGA") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        panel.border = element_rect(fill=NA, color="black"),  # Add border
        legend.position = "none"  # Remove legend
      )
    
    girafe(ggobj = p, width = 8, height = 7)
  })
  output$lineChartMelbourneAverage <- renderGirafe({
    # Filter data for Melbourne Lga
    melbourne_total_data <- total_data[total_data$Lga == "Melbourne", ]
    melbourne_salary_data <- salary_data[salary_data$Lga == "Melbourne", ]
    
    # Convert columns to numeric after removing non-numeric characters
    numeric_total_data <- apply(melbourne_total_data[,2:6], 2, function(x) as.numeric(gsub("[^0-9]", "", x)))
    numeric_salary_data <- apply(melbourne_salary_data[,2:6], 2, function(x) as.numeric(gsub("[^0-9]", "", x)))
    
    # Calculate the yearly average for job numbers (divided by 10) and salary
    avg_job_melbourne <- colMeans(numeric_total_data, na.rm = TRUE) / 10
    avg_salary_melbourne <- colMeans(numeric_salary_data, na.rm = TRUE)
    
    # Combine into a single data frame for plotting
    combined_data <- data.frame(
      Year = rep(2015:2019, 2),
      Type = factor(c(rep("Jobs", 5), rep("Salary", 5)), levels = c("Jobs", "Salary")),
      Value = c(avg_job_melbourne, avg_salary_melbourne)
    )
    
    # Plot the data
    p_melbourne_avg <- ggplot(combined_data, aes(x = Year, y = Value, color = Type, group = Type)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
      labs(title = "Average Jobs and Salary in Lga of Melbourne (2015-2019)",
           x = NULL,
           y = "Average value") +
      theme(
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text.x = element_text(color = "white", size = 16),
        axis.text.y = element_text(color = "white", size = 16),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        plot.title = element_text(color = "white", size = 20)
      ) +
      scale_color_manual(values = c("Jobs" = "lightblue", "Salary" = "red"))
    
    girafe(ggobj = p_melbourne_avg, width = 9, height = 7)
    
    
  })
  
  
  

  
}

  


#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
