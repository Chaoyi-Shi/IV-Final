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
library(shinydashboard)
library(RColorBrewer)
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

# Load the merged data
merged_data <- readxl::read_excel("data/modified_victoria_income_geo_data.xlsx")
geo_data <- geojsonio::geojson_read("data/cleaned_VIC_LGA.geojson", what = "sp")
spdf <- SpatialPolygonsDataFrame(geo_data, merged_data)
centroids <- gCentroid(spdf, byid=TRUE)

### Salary and jobs data
industry_data <- read.csv("data/Number of jobs by Industry 2019.csv", header = TRUE)
industry_data["Area"] = industry_data["X.Area"] 
industry_data$Area <- gsub("^X\\.", "", industry_data$Area)
jobs_salary_data <- read.csv("data/Number of Jobs and salary.csv", stringsAsFactors = FALSE)

total_data <- read.csv("data/Number of Jobs and salary.csv", header = TRUE)
salary_data <- read.csv("data/Income By Gender and area.csv", header = TRUE)

##################
# USER INTERFACE #
##################
  
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "sketchy"),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$title("City of Melbourne's Population and Socioeconomic Development"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$style(HTML("
      body, html {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: scroll;
      }

      #scaling-container {
        width: 1920px;
        height: 1080px;
        transform-origin: center center;
        transition: transform 0.2s;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function(){
        $(window).on('resize', function(){
            var scaleFactorWidth = window.innerWidth / 1920;
            var scaleFactorHeight = window.innerHeight / 1080;
            var scaleFactor = Math.min(scaleFactorWidth, scaleFactorHeight);
            $('#scaling-container').css('transform', 'scale(' + scaleFactor + ')');
        }).trigger('resize');
      });
    "))
  ),
  tags$div(id="scaling-container",
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
    
  "))),
  id='myPage',
  title="City of Melbourne's Population and Socioeconomic Development",
  tabPanel("Income", id = "income-map-tab",
           fluidPage(
             fluidRow(
               column(width = 6, valueBoxOutput("LGA_Name")),
               column(width = 6, div(class = "income-box", valueBoxOutput("Income")))
             ),
             tags$h4("User can click other LGA to get comparison with Melbourne", 
                     style = "color: white; padding: 1vh; text-align: center; margin-top: 2vh;"),
             tags$style(HTML("
                 .income-box {
                   margin-left: 20.83vw; 
                 }
                 ")),
             leafletOutput("map", height = "60vh"),
             absolutePanel(
               style = "top: 6.48vh; left: 50%; transform: translate(-50%, 0); z-index: 1000;",
               selectInput("data_choice", "Select Data:", 
                           choices = c("Mean Income" = "Mean $",
                                       "Median Income" = "Median $",
                                       "Gini Coefficient" = "Gini coefficient coef.",
                                       "Number of Earners" = "Earners (persons)"))
             ),
             
             absolutePanel(
               id = "pieChartsPanel", width = "25vw", height = "35vh",
               style = "left: 10vw; top: 14%; z-index: 1000; background-color: rgba(255, 255, 255, 0);", 
               plotlyOutput("melbourne_pie", height = "100%", width = "100%"),  
               plotlyOutput("pieplot", height = "100%", width = "100%")        
             ),
             
             absolutePanel(
               id = "barChartPanel", width = "25vw", height = "40vh",
               style = "right: 6vw; top: 25%; z-index: 1000;",
               plotlyOutput("comparison_plot", height = "100%", width = "100%")
             )
           )
           
           
  ),
  
  # Melbourne's Housing & Population Study Tab
  tabPanel(" Housing & Population",
           div(id = "tableauVizContainer", style = "height:500px;"),
           uiOutput("embedTableauViz")
  ),
  
  # Population Tab
  tabPanel("Population",
           fluidPage(
             absolutePanel(
               style = "top: 13vh; left: 45vw; width: 55vw; height: 85vh;",
               leafletOutput("population_map", height = "100%")
             ),
             absolutePanel(
               style = "top: 8vh; left: 5vw; width: 35vw;",
               useShinyjs(),
               sliderInput(
                 "year", "Select Year:",
                 min = 2011, max = 2021, value = 2011, step = 1
               ),
               actionButton("start_stop", "Start The Animation")
             ),
             absolutePanel(
               style = "top: 30vh; left: 3vw; width: 35vw; height: 70vh;",
               plotlyOutput("genderAgePlot", height = "100%")
             )
           )
  ),
  tabPanel("Job & Salary", 
           absolutePanel(
             style = "top: 6.5vh; left: 1vw;",
             selectInput(inputId = "selectedYear", label = "Select Year", 
                         choices = c("2016" = "X2016.17", 
                                     "2017" = "X2017.18", 
                                     "2018" = "X2018.19", 
                                     "2019" = "X2019.20"),
                         selected = "X2019.20")
           ),
           absolutePanel(
             style = "top: 6.5vh; left: 19vw;",
             selectInput("selected_area", "Area Choose: ", choices = sort(unique(industry_data$Area[industry_data$Lga == "Melbourne"])))
           ),
           absolutePanel(
             style = "top: 14vh; left: 5vw; width: 40vw; height: 35vh;",
             girafeOutput("rankedLgaChart")
           ),
           absolutePanel(
             style = "top: 6.8vh; left: 50vw; width: 25vw; height: 9vh;",
             girafeOutput("spiralBarChart")
           ),
           absolutePanel(
             style = "top: 58vh; left: 5vw; width: 40vw; height: 9vh;",
             girafeOutput("job_pie_chart")
           ),
           absolutePanel(
             style = "top: 58vh; left: 50vw; width: 40vw; height: 9vh;",
             girafeOutput("combinedLineChart")
           )
  ),
  # Define the "About" dropdown menu
  aboutMenu <- navbarMenu(
    "About",
    tabPanel(
      "Assingment2 Description",
      br(),
      h1(HTML("<b>Assingment2 Description</b>"), style = "text-align:center"),
      br(),
      span(h4("This project consists of a visualization and analysis of COVID-19 
            outbreaks throughout the American continent. To begin with, R 
            libraries were prepared and datasets related to the outbreak were 
            loaded, including cases of COVID-19 reported from countries across 
            each continent and deaths reported from US states. Filtering, 
            summarizing, and grouping the outbreak data were part of the data 
            processing phase. It is designed with multiple tabs (for example, 
            distribution of cases by country, intensity and growth rate of the 
            outbreak, number of deaths by state) for interactive viewing. A 
            variety of visualizations are dynamically rendered on the Shiny server 
            side (e.g., bar charts, maps, and line graphs). Furthermore, 
            the project contains links to data sources and descriptions of the visualizations.")),
      br(),
      img(src = "https://www.psa.org.au/wp-content/uploads/2020/04/CVOID_Cover.png", height = 300, width = 600,class="centered-image"), # 你可以根据需要调整height和width的值
      tags$style(HTML("
    .centered-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
    }
  ")),
    )
  ),
  tags$div(
    style = "text-align: center; margin-top: 20px; color: white; position: fixed; bottom: 0; width: 100%; background-color: black; padding: 10px 0;",
    "© 2023  City of Melbourne's Population and Socioeconomic Development. All rights reserved."
  )
)




################
# SHINY SERVER #
################

server <- function(input, output,session) {
  selected_LGA <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    # Create a color palette based on user's choice
    pal <- colorNumeric("Blues", domain = na.omit(spdf[[input$data_choice]]))
    # Filter only Melbourne data for the label
    melbourne_data <- spdf[spdf$`LGA NAME` == "Melbourne", ]
    #print(melbourne_data)
    
    myCustomIcon <- makeIcon(
      iconUrl = "m.png",  
      iconWidth = 70,                  
      iconHeight = 26,
      iconAnchorX = 40,                   
      iconAnchorY = 15                 
    )
    
    leaflet(data = spdf) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(spdf[[input$data_choice]]), "gray", pal(spdf[[input$data_choice]])),
        fillOpacity = 0.7,  
        color = "white",
        weight = 1,
        layerId = ~spdf$`LGA NAME`,
        popup = paste0("<strong>LGA: </strong>", spdf$`LGA NAME`, 
                       "<br><strong>", input$data_choice, ": </strong>", 
                       ifelse(is.na(spdf[[input$data_choice]]), "No useful information", spdf[[input$data_choice]]))
      )%>%
      addMarkers(
        data = melbourne_data,
        lat = ~-37.8136, 
        lng = ~144.9630,
        icon = myCustomIcon
      )%>%
      addLegend(pal = pal, values = ~spdf[[input$data_choice]], title = input$data_choice, position = "bottomright") 
  })
  
  observeEvent(input$map_shape_click, {
    click_data <- input$map_shape_click
    
    if (!is.na(click_data$id) && !is.null(click_data$id)) {
      
      # update selected lga
      selected_LGA(click_data$id)
    } else {
      selected_LGA("undefined")
    }
    
    # according selected_LGA() find corresponding row index
    row_index <- which(merged_data$`LGA NAME` == selected_LGA())
    
    quartile_data <- merged_data[row_index, c("Lowest Quartile %", "Second Quartile %", "Third Quartile %", "Highest Quartile %")]
    
    
    # Render a pie chart for the quartiles
    output$pieplot <- renderPlotly({
      # Define a vector of colors you wish to use for your pie slices
      colors_vector <- c("#45a7ed", "#98cef5", "#f598b4", "#f7cdda") 
      # if choose “MELBOURNE”，return none, no plotting
      if (selected_LGA() == "MELBOURNE") {
        return(NULL)
      }
      p <- plot_ly(quartile_data, 
                   labels = ~names(quartile_data), 
                   values = ~unlist(quartile_data), 
                   type = 'pie', 
                   hole = 0.6,
                   marker = list(colors = colors_vector, line = list(color = "#FFFFFF", width = 2)),
                   hoverinfo = 'label+percent', 
                   textinfo = 'percent',
                   hoveron = 'points+fills', 
                   insidetextfont = list(color = 'white', size = "1vw")
      ) %>%
        layout(title = list(text = "Income Quartile Distribution", x = 0.5, y = 1.05, font = list(size = 14, color = "white")), 
               margin = list(l = 20, r = 20, b = 20, t = 60), 
               legend = list(font = list(size = 10, color = "white"), x = 0.32, y = 0.45, bgcolor = "rgba(0,0,0,0)"),
               showlegend = TRUE,
               annotations = list(
                 list(text = selected_LGA(), 
                      x = 0.5,
                      y = 0.63,
                      xref = 'paper', 
                      yref = 'paper', 
                      showarrow = FALSE, 
                      font = list(size = "1.2vw", color = "white")
                 )
               ),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
  })
  
  # Render a pie chart for Melbourne's quartiles
  output$melbourne_pie <- renderPlotly({
    # Get Melbourne's data
    melbourne_data <- merged_data[merged_data$`LGA NAME` == "MELBOURNE", c("Lowest Quartile %", "Second Quartile %", "Third Quartile %", "Highest Quartile %")]
    
    # Define a vector of colors you wish to use for your pie slices
    colors_vector <- c("#45a7ed", "#98cef5", "#f598b4", "#f7cdda") 
    
    p <- plot_ly(melbourne_data, 
                 labels = ~names(melbourne_data), 
                 values = ~unlist(melbourne_data), 
                 type = 'pie', 
                 hole = 0.6,
                 marker = list(colors = colors_vector, line = list(color = "#FFFFFF", width = 2)),
                 hoverinfo = 'label+percent', 
                 textinfo = 'percent',
                 hoveron = 'points+fills', 
                 insidetextfont = list(color = 'white')
    )%>%
      layout(title = list(text = "Melbourne Income Quartile Distribution", x = 0.5, y = 1.05, font = list(size = 14, color = "white")), 
             margin = list(l = 20, r = 20, b = 20, t = 60), 
             legend = list(font = list(size = 10, color = "white"), x = 0.32, y = 0.45, bgcolor = "rgba(0,0,0,0)"),
             showlegend = TRUE,
             annotations = list(
               list(text = "MELBOURNE", 
                    x = 0.5,
                    y = 0.63,
                    xref = 'paper', 
                    yref = 'paper', 
                    showarrow = FALSE, 
                    font = list(size = 12, color = "white")
               )
             ),
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  output$comparison_plot <- renderPlotly({
    # no plotting if don't choose any lga
    if (is.null(selected_LGA()) ) {
      return(NULL)
    }
    
    # get selected LGA and "Melbourne"
    comparison_data <- merged_data[merged_data$`LGA NAME` %in% c(selected_LGA(), "MELBOURNE"), ]
    
    p <- plot_ly(data = comparison_data, x = ~`LGA NAME`, y = ~`Top 1% %`, type = "bar", name = "Top 1%", marker = list(color = "#c5e0fa"))
    p <- p %>% add_trace(y = ~`Top 5% %`, name = "Top 5%", marker = list(color = "#74b3f2"))
    p <- p %>% add_trace(y = ~`Top 10% %`, name = "Top 10%", marker = list(color = "#1887f5"))
    
    p %>% layout(barmode = 'group',
                 title = list(text = "Comparison of Selected LGA and Melbourne", font = list(color = "white")),
                 legend = list(font = list(color = "white")),
                 xaxis = list(title = "", tickfont = list(color = "white")),
                 yaxis = list(title = "High Income %", tickfont = list(color = "white"),titlefont = list(color = "white")),
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor = 'rgba(0,0,0,0)'
    )
  })
  
  # Use the selected_LGA reactiveVal in the renderValueBox function
  output$LGA_Name <- renderValueBox({
    valueBox(
      paste("Selected LGA: ", selected_LGA()), "LGA Selection"
    )
  })
  
  output$Income <- renderValueBox({
    # use selected_LGA()find corresonding row index 
    row_index <- which(spdf@data$`LGA NAME` == selected_LGA())
  
    if(length(row_index) == 0) {
      return(valueBox(paste(input$data_choice, ": undefined"), "Income Selection"))
    }
    
    selected_value <- spdf@data[row_index, as.character(input$data_choice)]
    
    selected_value <- selected_value[1]
    
    if (is.na(selected_value)) {
      valueBox(
        paste(input$data_choice, ": undefined"), "Income Selection"
      )
    } else {
      valueBox(
        paste(input$data_choice, ": ", selected_value), "Income Selection"
      )
    }
  })
  
  current_year <- reactiveVal(2011)
  is_running <- reactiveVal(FALSE)
  
  auto_increment_timer <- reactiveTimer(3000)  # 3 seconds timer
  
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
      domain = c(70000, 180000)
    )
    sur_data = lga_data %>%
      filter(LGA.Name %in% c("MELBOURNE", "BOROONDARA","HOBSONS BAY",
                             "MOONEE VALLEY", "MARIBYRNONG","PORT PHLILLIP",
                             "BAYSIDE", "GLENEIRA","YARRA","STONNINGTON",
                             "MORELAND", "DAREBIN"))
    mel_data = lga_data %>%
      filter(LGA.Name =="MELBOURNE")
    
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(
        data = sur_data,
        fillColor = ~pal(get(target_col)),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        label = ~paste(LGA.Name, "<br>Population: ", get(target_col))
      ) %>%
      addLegend(
        pal = pal,
        values = c(70000, 180000),
        title = "Population",
        position = "bottomright"
      )%>%
      addLabelOnlyMarkers(
        data = mel_data,
        lat = ~-37.8136, 
        lng = ~144.9631,  
        label = ~paste("Melbourne Population:", as.character(get(target_col))),
        labelOptions = labelOptions(noHide = TRUE, style = list("font-size" = "16px",
                                                                "background-color" = "#d9f0fa", 
                                                                "border" = "2px",
                                                                "color" = "black"))
      )
  })
  
  output$embedTableauViz <- renderUI({
    # Tableau viz URL
    viz_url <- "https://public.tableau.com/views/V2_16977821675040/AnalysisofDwellingsFloorSpaceandPopulationTrendsinMelbourne2021-2041?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link"
    
    # use JavaScript Embedding API's initViz method embed Tableau viz
    script <- sprintf('
    <script>
      function initViz() {
        var containerDiv = document.getElementById("tableauVizContainer");
        var vizUrl = "%s";
        var options = {
          hideTabs: true, 
          width: "1700px",  
          height: "920px"  
        };
        
        var viz = new tableau.Viz(containerDiv, vizUrl, options);
      }
      initViz();
    </script>
  ', viz_url)
    
    HTML(script)
  })
  
  output$genderAgePlot <- renderPlotly({
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
    
    pop_range <- range(-180000, 180000)
    pop_range_seq <- seq(-180000, 180000, by = 4000)
    
    p <- ggplotly(
      ggplot(combined_data_long, aes(x = Age_Group_Sort, y = Population, fill = Gender, text = paste("Age Group: ", Age_Group, "<br>Population: ", abs(Population)))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        scale_y_continuous(labels = abs, expand = c(0, 0), breaks = pop_range_seq) +
        scale_fill_manual(values = c("Male" = "#bbeafc", "Female" = "pink"), name = "") +
        coord_flip() +
        facet_wrap(. ~ Gender, scale = "free_x", strip.position = "bottom") +
        labs(title = "Age-Gender Pyramid of Melbourne",
             x = "Age Group",
             y = "Population",
             fill = "Gender") +
        theme_minimal() +
        theme(legend.position = "bottom",
              panel.spacing.x = unit(0, "pt"),
              panel.spacing.y = unit(0, "pt"),
              panel.background = element_rect(fill = "transparent", color = NA),
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(color = "white"),
              axis.text.x = element_text(color = "white"),  
              axis.text.y = element_text(color = "white")   
        )  
    )
    p_plotly <- ggplotly(p)
    
    # Adjust the layout for size
    p_plotly <- layout(p_plotly, autosize = F, width = 700, height = 600)
    
  })

  # job and salary panel start
  output$combinedLineChart <- renderGirafe({
    
    # --- First Part: Preparing selected area data ---
    selected_total_data <- total_data[total_data$Area == input$selected_area, ]
    long_total_data <- melt(selected_total_data, id.vars = "Area")
    long_total_data$type <- "Jobs"
    long_total_data <- long_total_data[long_total_data$variable != "Lga", ]
    
    selected_salary_data <- salary_data[salary_data$Area == input$selected_area, ]
    long_salary_data <- melt(selected_salary_data, id.vars = "Area")
    long_salary_data$type <- "Salary"
    long_salary_data <- long_salary_data[long_salary_data$variable != "Lga", ]
    
    combined_total_data <- rbind(long_total_data, long_salary_data)
    combined_total_data$value <- gsub(",", "", combined_total_data$value)
    combined_total_data$value <- as.numeric(combined_total_data$value)
    combined_total_data$Year <- as.numeric(gsub("^X(\\d{4}).*", "\\1", combined_total_data$variable))
    combined_total_data$Source <- "Selected Area"
    
    # --- Second Part: Preparing Melbourne data ---
    melbourne_total_data <- total_data[total_data$Lga == "Melbourne", ]
    melbourne_salary_data <- salary_data[salary_data$Lga == "Melbourne", ]
    numeric_total_data <- apply(melbourne_total_data[,2:6], 2, function(x) as.numeric(gsub("[^0-9]", "", x)))
    numeric_salary_data <- apply(melbourne_salary_data[,2:6], 2, function(x) as.numeric(gsub("[^0-9]", "", x)))
    avg_job_melbourne <- colMeans(numeric_total_data, na.rm = TRUE) / 10
    avg_salary_melbourne <- colMeans(numeric_salary_data, na.rm = TRUE)
    
    combined_data_melbourne <- data.frame(
      Year = rep(2015:2019, 2),
      Type = factor(c(rep("Jobs", 5), rep("Salary", 5)), levels = c("Jobs", "Salary")),
      Value = c(avg_job_melbourne, avg_salary_melbourne),
      Source = "Melbourne"
    )
    
    # Adjusting combined_total_data
    adjusted_total_data <- combined_total_data %>%
      select(Year, type, value, Source) %>%
      rename(Type = type, Value = value)
    
    # Now, rbind the two datasets together
    all_data <- rbind(adjusted_total_data, combined_data_melbourne)
    
    # --- Plotting ---
    p_combined <- ggplot(all_data, aes(x = Year, y = Value, color = Type, group = interaction(Type, Source))) +
      geom_line(linewidth = 1.5) +  # Adjusting size here to make line thinner
      geom_point() +
      geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
      facet_wrap(~ Source, scales = "free_y") +
      labs(title = "Average Number of Jobs and Salaries (2015-2019)",
           x = NULL,
           y = "Average value") +
      theme(
        panel.grid.major = element_blank(),
        panel.spacing.x = unit(0, "pt"),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
        text = element_text(color = "white"),
        axis.text.x = element_text(color = "white", size = 20),
        axis.text.y = element_text(color = "white", size = 20),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "black",size = 16),
        legend.title = element_text(color = "black", size = 16),
        plot.title = element_text(color = "white", size = 24),
        strip.text = element_text(size = 20, color = "black")  # Adjusting strip text size and color)
      ) +
      scale_color_manual(values = c("Jobs" = "lightblue", "Salary" = "lightpink"))
      girafe(ggobj = p_combined, width = 14, height = 8)
  })
  
  pastel1_colors <- brewer.pal(9, "Pastel1")
  spectral_colors <- brewer.pal(11, "Spectral")
  combined_palette <- c(spectral_colors, pastel1_colors)  
  output$job_pie_chart <- renderGirafe({
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
      labs(title = paste("Number of Jobs in different Industry"), 
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        legend.position = "left",
        panel.spacing.x = unit(0, "pt"),
        panel.spacing.y = unit(0, "pt"),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black", color = NA),
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
  
  output$spiralBarChart <- renderGirafe({
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
      scale_fill_brewer(palette = "Spectral") + 
      theme_minimal()+
      theme(
        panel.spacing.x = unit(0, "pt"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold", color = "white"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white", size = 30),
        legend.position = "none"
      ) +
      labs(y = NULL, x = NULL, title = "Specific Area Average Number of Jobs in Melbourne")
      girafe(ggobj = p, width = 15, height = 15.51)
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
      scale_fill_manual(values = c("Melbourne" = "lightpink", "Others" = "#bbeafc")) +  # Manual color assignment for Melbourne
      labs(y = "Total Number of Jobs", x = "LGA", title = "Average Number of Jobs in Melbourne and surrounding LGA") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "white"), # Rotate x-axis labels by 45 degrees
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        panel.border = element_rect(fill=NA, color="black"),  # Add border
        legend.position = "none"  # Remove legend
      )
    
    girafe(ggobj = p, width = 8, height = 4)
    
  })
}


#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
