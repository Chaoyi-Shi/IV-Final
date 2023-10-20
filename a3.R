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


# Load the GeoJSON data
lga_data <- st_read("population data/merged_vic_lga.geojson")
lga_data <- st_transform(lga_data, 4326)
lga_data <- lga_data[, -(1:11)]

# Load the GEOM90007 Tableau in Shiny library
source('tableau-in-shiny-v1.0.R')

# Load the birthrate geojson data
lga_data <- st_read("population data/merged_vic_lga.geojson")
lga_data <- st_transform(lga_data, 4326)
lga_data <- lga_data[, -(1:11)]

#age and gender data 
age_sex_male_data = read.xlsx("population data/Population estimates by age and sex, by LGA, 2001 to 2022.xlsx",sheet=2,startRow = 9)
age_sex_female_data = read.xlsx("population data/Population estimates by age and sex, by LGA, 2001 to 2022.xlsx",sheet=3,startRow = 9)

# filter out only the Victoria's data
age_sex_male_data <- age_sex_male_data[age_sex_male_data$`S/T.name`=="Victoria",]
age_sex_female_data <- age_sex_female_data[age_sex_female_data$`S/T.name`=="Victoria",]

# Remove rows where any column contains "None"
age_sex_male_data <- age_sex_male_data %>%
  filter_all(all_vars(. != "None" & . != "NA"))

age_sex_female_data <- age_sex_female_data %>%
  filter_all(all_vars(. != "None" & . != "NA"))


# Load the merged data
merged_data <- readxl::read_excel("merged_victoria_income_geo_data.xlsx")
geo_data <- geojsonio::geojson_read("VIC LGA.geojson", what = "sp")
spdf <- SpatialPolygonsDataFrame(geo_data, merged_data)
centroids <- gCentroid(spdf, byid=TRUE)


##################
# USER INTERFACE #
##################
ui <- fluidPage(
  tags$script(src = "https://public.tableau.com/javascripts/api/tableau-2.min.js"),
  tags$style("
    .navbar {
      position: absolute;
      top: 10px;
      right: 10px;
      width: 200px;
      z-index: 1000;
    }
  "),
  navbarPage("",
             tabPanel("Income Map",
                      fluidPage(
                        leafletOutput("map", height = "100vh"),
                        absolutePanel(top = 10, left = 10,
                                      selectInput("data_choice", NULL, 
                                                  choices = c("Mean Income" = "Mean $",
                                                              "Median Income" = "Median $",
                                                              "Gini Coefficient" = "Gini coefficient coef.",
                                                              "Number of Earners" = "Earners (persons)")),
                                      plotlyOutput("barplot", height = "300px", width = "300px")
                        ),
                        absolutePanel(bottom = 10, right = 10, width = "20%", 
                                      tags$div(id = "legend", style = "background-color: white; padding: 10px;")
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
                  
                  absolutePanel( top = 300,     # Position from the top of the page (in pixels)
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

server <- function(input, output) {
  output$map <- renderLeaflet({
    # Create a color palette based on user's choice
    pal <- colorNumeric("YlOrRd", domain = na.omit(spdf[[input$data_choice]]))
    
    leaflet(data = spdf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(spdf[[input$data_choice]]), "gray", pal(spdf[[input$data_choice]])), 
        fillOpacity = 0.5,  # Adjusted opacity
        weight = 1,
        popup = paste0("<strong>LGA: </strong>", spdf$`LGA NAME`, 
                       "<br><strong>", input$data_choice, ": </strong>", spdf[[input$data_choice]])
      ) %>%
      addLegend(pal = pal, values = ~spdf[[input$data_choice]], title = input$data_choice, position = "bottomright") %>%
      addLabelOnlyMarkers(lng = coordinates(centroids)[, 1], lat = coordinates(centroids)[, 2], label = ~spdf$`LGA NAME`, 
                          labelOptions = labelOptions(noHide = TRUE, direction = "center", style = list("color" = "black", "fontSize" = "8px")))
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
      palette = "YlOrRd",
      domain = lga_data[[target_col]]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
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
