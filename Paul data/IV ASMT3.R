library("shiny")
library("leaflet")
library('dplyr')
library("ggplot2")
library("ggiraph")
library("reshape2")

setwd("~/Desktop/IV ASMT Paul")
industry_data <- read.csv("Number of jobs by Industry 2019.csv", header = TRUE)
industry_data["Area"] = industry_data["X.Area"] 
industry_data$Area <- gsub("^X\\.", "", industry_data$Area)
jobs_salary_data <- read.csv("Number of Jobs and salary.csv", stringsAsFactors = FALSE)


total_data <- read.csv("Number of Jobs and salary.csv", header = TRUE)
salary_data <- read.csv("Income By Gender and area.csv", header = TRUE)

##################
# USER INTERFACE #
##################

# ... [previous code] ...

ui <- fluidPage(
  tags$div("LGA: Local Government Area", style = "position: absolute; top: 60px; left: 10px; color: white; font-size: 24px; z-index: 100;"),  # Add this line
  tags$div("More Specific Area: Statistic Area Level 2", style = "position: absolute; top: 90px; left: 10px; color: white; font-size: 24px; z-index: 100;"),  # Add this line
  tags$style(HTML("
    body {
      background-color: black;
      margin: 0 !important;
      padding: 0 !important;
    }
    .shiny-output-error {
      display: none;
    }
  ")),
  titlePanel("Jobs and salary in Victoria by areas in 2019"),
  
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
  div(style = "clear: both;")
)

# ... [other code] ...






################
# SHINY SERVER #
################

server <- function(input, output,session) {
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
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = "bold", color = "white"),
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
    
    girafe(ggobj = p,  width = 15.0, height = 15.29)
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
      scale_color_manual(values = c("Jobs" = "lightblue", "Salary" = "red"))+
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
# RUN SHINY #
#############

shinyApp(ui, server)
