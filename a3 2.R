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


##################
# USER INTERFACE #
##################
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "sketchy"),
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
    
  ")),
  id='myPage',
  title="City of Melbourne's Population and Socioeconomic Development",
  tabPanel("Income Map", id = "income-map-tab",
           fluidPage(
             fluidRow(
               column(width = 6, valueBoxOutput("LGA_Name")),
               column(width = 6, div(class = "income-box", valueBoxOutput("Income")))
             ),
             tags$style(HTML("
                          .income-box {
                            margin-left: 400px;  /* 调整这个值来增加或减少偏移量 */
                          }
                        ")),
             leafletOutput("map", height = "100vh"),
             tags$div(
               style = "position: absolute; top: 60px; left: 50%; transform: translate(-50%, 0); z-index: 1000;",
               selectInput("data_choice", "Select Data:", 
                           choices = c("Mean Income" = "Mean $",
                                       "Median Income" = "Median $",
                                       "Gini Coefficient" = "Gini coefficient coef.",
                                       "Number of Earners" = "Earners (persons)"))
             ),
             tags$div(
               style = "position: absolute; left: 10px; top: 50%; transform: translateY(-50%); z-index: 1000; width: 400px; height: 500px; background-color: rgba(255, 255, 255, 0);", 
               plotlyOutput("melbourne_pie"),  # Melbourne's pie chart
               plotlyOutput("barplot")         # Selected LGA's pie chart
             )
             ,
             tags$div(
               style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); z-index: 1000; width: 400px; height: 500px;",
               plotlyOutput("comparison_plot"),
             )
           )
  ),
  
  # Melbourne's Housing & Population Study Tab
  tabPanel("Melbourne's Housing & Population Study",
           div(id = "tableauVizContainer", style = "height:500px;"),
           uiOutput("embedTableauViz")
  ),
  
  # Population Tab
  tabPanel("Population",
           fluidPage(
             absolutePanel(top = 70, left = 650, width = 600, height = 500, leafletOutput("population_map", height = "90vh")),
             absolutePanel(top = 70, left = 40,
                           useShinyjs(),
                           sliderInput(
                             "year", "Select Year:",
                             min = 2011, max = 2021, value = 2011, step = 1
                           ),
                           actionButton("start_stop", "Start The Animation")
             ),
             absolutePanel(top = 220, left = 40, width = 550, height = 650, plotlyOutput("genderAgePlot"))
           )
  )
  
)




################
# SHINY SERVER #
################

server <- function(input, output,session) {
  
  # 创建一个reactiveVal存储选定的LGA名字
  selected_LGA <- reactiveVal(NULL)
  
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
        layerId = ~spdf$`LGA NAME`,
        popup = paste0("<strong>LGA: </strong>", spdf$`LGA NAME`, 
                       "<br><strong>", input$data_choice, ": </strong>", 
                       ifelse(is.na(spdf[[input$data_choice]]), "No useful information", spdf[[input$data_choice]]))
      ) %>%
      addLegend(pal = pal, values = ~spdf[[input$data_choice]], title = input$data_choice, position = "bottomright") 
  })
  
  observeEvent(input$map_shape_click, {
    click_data <- input$map_shape_click
    
    if (!is.na(click_data$id) && !is.null(click_data$id)) {
      
      # 更新当前选中的LGA名字（不改变之前的功能）
      selected_LGA(click_data$id)
    } else {
      selected_LGA("undefined")
    }
    
    # 根据selected_LGA()找到对应的行索引
    row_index <- which(merged_data$`LGA NAME` == selected_LGA())
    
    # 提取quartile数据
    quartile_data <- merged_data[row_index, c("Lowest Quartile %", "Second Quartile %", "Third Quartile %", "Highest Quartile %")]
    
    
    # Render a pie chart for the quartiles
    output$barplot <- renderPlotly({
      # 如果选择的是“MELBOURNE”，直接返回NULL，不绘制图形
      if (selected_LGA() == "MELBOURNE") {
        return(NULL)
      }
      p <- plot_ly(quartile_data, 
                   labels = ~names(quartile_data), 
                   values = ~unlist(quartile_data), 
                   type = 'pie', 
                   hole = 0.6,
                   marker = list(line = list(color = "#FFFFFF", width = 2)), # 白色边框
                   hoverinfo = 'label+percent', 
                   textinfo = 'percent',
                   hoveron = 'points+fills', 
                   insidetextfont = list(color = 'white')
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
                      font = list(size = 12, color = "white")
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
    
    p <- plot_ly(melbourne_data, 
                 labels = ~names(melbourne_data), 
                 values = ~unlist(melbourne_data), 
                 type = 'pie', 
                 hole = 0.6,
                 marker = list(line = list(color = "#FFFFFF", width = 2)), 
                 hoverinfo = 'label+percent', 
                 textinfo = 'percent',
                 hoveron = 'points+fills', 
                 insidetextfont = list(color = 'white')
    ) %>%
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
    # 如果未选择任何LGA，则不绘图
    if (is.null(selected_LGA()) ) {
      return(NULL)
    }
    
    # 提取当前选中的LGA和"Melbourne"
    comparison_data <- merged_data[merged_data$`LGA NAME` %in% c(selected_LGA(), "MELBOURNE"), ]
    
    # 使用plot_ly创建对比图
    p <- plot_ly(data = comparison_data, x = ~`LGA NAME`, y = ~`Top 1% %`, type = "bar", name = "Top 1%", marker = list(color = "lightblue"))
    
    p <- p %>% add_trace(y = ~`Top 5% %`, name = "Top 5%", marker = list(color = "lightcoral"))
    
    p <- p %>% add_trace(y = ~`Top 10% %`, name = "Top 10%", marker = list(color = "lightgreen"))
    
    p %>% layout(barmode = 'group',
                 title = list(text = "Comparison of Selected LGA and Melbourne", font = list(color = "white")),
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
  
  current_year <- reactiveVal(2011)
  is_running <- reactiveVal(FALSE)
  
  auto_increment_timer <- reactiveTimer(3000)  # 2 seconds timer
  
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
      domain = c(100000, 180000)
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
        values = c(100000, 180000),
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
  
  output$embedTableauViz <- renderUI({
    # 在此处指定 Tableau viz 的 URL
    viz_url <- "https://public.tableau.com/views/V2_16977821675040/AnalysisofDwellingsFloorSpaceandPopulationTrendsinMelbourne2021-2041?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link"
    
    # 使用 JavaScript Embedding API 的 initViz 方法嵌入 Tableau viz
    script <- sprintf('
    <script>
      function initViz() {
        var containerDiv = document.getElementById("tableauVizContainer");
        var vizUrl = "%s";
        var options = {
          hideTabs: true, // 隐藏 Tableau 选项卡
          width: "1700px",  // 设置宽度
          height: "940px"  // 设置高度
        };
        
        var viz = new tableau.Viz(containerDiv, vizUrl, options);
      }
      initViz();
    </script>
  ', viz_url)
    
    # 返回 script
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
    
    p <- ggplotly(
      ggplot(combined_data_long, aes(x = Age_Group_Sort, y = Population, fill = Gender, text = paste("Age Group: ", Age_Group, "<br>Population: ", abs(Population)))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        scale_y_continuous(labels = abs, expand = c(0, 0)) +
        scale_fill_manual(values = c("Male" = "#68b9f7", "Female" = "pink"), name = "") +
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
              plot.background = element_rect(fill = "transparent", color = NA),
              text = element_text(color = "white"),
              axis.text.x = element_text(color = "white"),  
              axis.text.y = element_text(color = "white")   
        )  
    )
    
  })
  #################################################################################
}





#############
# Run Shiny #
#############
shinyApp(ui, server, options=list(launch.browser=TRUE))
