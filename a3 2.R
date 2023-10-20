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
             tabPanel("Population Income and Building Approvals", id = "combined-tab",
                      fluidPage(
                        fluidRow(
                          column(width = 6, valueBoxOutput("LGA_Name")),
                          column(width = 6, div(class = "income-box", valueBoxOutput("Income")))
                        ),
                        tags$style(HTML("
                            .income-box {
                                margin-left: 400px;  
                            }
                        ")),
                        fluidRow(
                          column(width = 12, 
                                 leafletOutput("map", height = "50vh")
                          )
                        ),
                        fluidRow(
                          column(width = 12, 
                                 div(id = "tableauVizContainer" ,style = "height:700px"),
                                 uiOutput("embedTableauViz")
                          )
                        ),
                        tags$div(
                          style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); z-index: 1000;",
                          selectInput("data_choice", "Select Data:", 
                                      choices = c("Mean Income" = "Mean $",
                                                  "Median Income" = "Median $",
                                                  "Gini Coefficient" = "Gini coefficient coef.",
                                                  "Number of Earners" = "Earners (persons)")
                          )
                        ),
                        tags$div(
                          style = "position: absolute; left: 50px; top: 30%; transform: translateY(-50%); z-index: 1000; width: 350px; height: 200px;",
                          plotlyOutput("barplot")
                        )
                      )
             ),
             tabPanel("Population and Road development",
                      
                      # Population content
                      fluidPage(
                        leafletOutput("birth_rate_map", height = "60vh"),
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
                            });
                        '),
                        absolutePanel( 
                          top = 240,     # Position from the top of the page (in pixels)
                          left = 40,    # Position from the left of the page (in pixels)
                          width = 300,   # Width of the panel (in pixels)
                          height = 280,  # Height of the panel (in pixels)
                          
                          # Render a plot within the absolute panel
                          plotOutput("genderAgePlot")
                        ),
                        
                        # Separator or Divider (optional, you can adjust the style as needed)
                        hr(style = "border-top: 1px solid #ccc;"),
                        
                        # Road infrastructure development content
                        div(id = "tableauVizRoad", style = "height:500px;"),
                        uiOutput("embedTableauVizRoad")
                      )       
             )
             
  )
)



################
# SHINY SERVER #
################

server <- function(input, output) {
  # 创建一个reactiveVal存储选定的LGA名字
  selected_LGA <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    # Create a color palette based on user's choice
    pal <- colorNumeric("Blues", domain = na.omit(spdf[[input$data_choice]]))
    
    leaflet(data = spdf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(spdf[[input$data_choice]]), "gray", pal(spdf[[input$data_choice]])), 
        fillOpacity = 0.7,  
        color = "white",
        weight = 1,
        layerId = ~spdf$`LGA NAME`, # 这是添加的部分
        popup = paste0("<strong>LGA: </strong>", spdf$`LGA NAME`, 
                       "<br><strong>", input$data_choice, ": </strong>", 
                       ifelse(is.na(spdf[[input$data_choice]]), "No useful information", spdf[[input$data_choice]]))
        
      ) %>%
      addLegend(pal = pal, values = ~spdf[[input$data_choice]], title = input$data_choice, position = "bottomright") 
  })
  
  # Add an observer for the click event
  observeEvent(input$map_shape_click, {
    click_data <- input$map_shape_click
    if (is.na(click_data$id) || is.null(click_data$id)) {
      selected_LGA("undefined")
    } else {
      selected_LGA(click_data$id)
    }
  })
  
  # Use the selected_LGA reactiveVal in the renderValueBox function
  output$LGA_Name <- renderValueBox({
    valueBox(
      paste("Selected LGA: ", selected_LGA()), "LGA Selection"
    )
  })
  
  output$Income <- renderValueBox({
    # 根据selected_LGA()找到对应的行索引
    row_index <- which(spdf@data$`LGA NAME` == selected_LGA())
    
    # 如果row_index为空，则直接返回"undefined"
    if(length(row_index) == 0) {
      return(valueBox(paste(input$data_choice, ": undefined"), "Income Selection"))
    }
    
    # 获取选中LGA在input$data_choice所指定的列中的值
    selected_value <- spdf@data[row_index, as.character(input$data_choice)]
    
    # 为了确保selected_value是一个单一的值，我们取向量的第一个元素
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
    viz_url <- "https://public.tableau.com/views/V2_16977821675040/Dashboard1?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link"
    
    # 使用 JavaScript Embedding API 的 initViz 方法嵌入 Tableau viz
    script <- sprintf('
    <script>
      function initViz() {
        var containerDiv = document.getElementById("tableauVizContainer");
        var vizUrl = "%s";
        var options = {
          hideTabs: true, // 隐藏 Tableau 选项卡
          width: "100%%",  // 设置宽度
          height: "100%%"  // 设置高度
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
          hideTabs: true, 
          width: "100%%",  
          height: "100%%"  
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
}




#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
