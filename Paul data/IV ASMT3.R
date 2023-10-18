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

ui <- fluidPage(
  titlePanel("Jobs and salary in Victoria by areas in 2019"),
  
  # Sidebar
  div(style = "float: left; width: 40%;",  # This CSS will make the sidebar float to the left and occupy 20% width
      sliderInput("lgaRange", 
                  "Select LGA Rank Range:", 
                  min = 1, 
                  max = length(unique(jobs_salary_data$Lga)), 
                  value = c(1, 10),
                  step = 1),
      selectInput("selected_LGA", "Select LGA:", choices = unique(jobs_salary_data$Lga)),
      selectInput("selected_area", "Select Area:", choices = unique(industry_data$Area)),
      girafeOutput("barChart"),
      girafeOutput("lineChart")
  ),
  
  # Main charts
  # Main charts
  div(style = "float: left; width: 60%;",  # This CSS will make the main content float to the left and occupy 80% width
      div(
        girafeOutput("rankedLgaChart"),
        style = "margin-bottom: 20px;"   # Adds a margin to the bottom of the first chart
      ),
      girafeOutput("interactiveBarChart")
  ),
  
  
  # Clear float to ensure proper alignment of subsequent elements
  div(style = "clear: both;")
)





################
# SHINY SERVER #
################

server <- function(input, output,session) {
  clicked_area_reactive <- reactive({
    input$clicked_area
  })
  
  observe({
    clicked_area <- input$clicked_area
    print(clicked_area)
  })
  
  output$barChart <- renderGirafe({
    selected_data <- industry_data[industry_data$Area == input$selected_area, ]
    selected_data <- na.omit(selected_data)  # Remove rows with NA values
    long_data <- melt(selected_data, id.vars="Area")
    
    # Filter out Lga variable
    long_data <- long_data[!long_data$variable %in% c("X.Area", "Lga", "Total"), ]
    
    # Create the tooltip_info column with same format as x-axis
    long_data$tooltip_info <- paste("Industry:", gsub("\\.", " ", long_data$variable), "<br>Jobs:", long_data$value)
    
    p <- ggplot(long_data, aes(x=variable, y=value, tooltip=tooltip_info)) +  
      geom_bar_interactive(stat="identity", aes(tooltip=tooltip_info, data_id=variable)) +
      scale_fill_manual(values=rainbow(length(unique(long_data$variable)))) +
      labs(title=paste("Jobs by Industry for", input$selected_area, "in 2019"), 
           x="Industry", y="Number of Jobs") +
      theme(
        axis.text.x = element_text(angle=45, hjust=1, size=10, face="bold"),
        axis.text.y = element_text(size=12),
        axis.line.x = element_line(color="black", size=1),
        legend.position = "none",
        panel.border = element_rect(fill=NA, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      scale_x_discrete(labels = function(x) gsub("\\.", " ", x))
    
    girafe(ggobj=p, width=9, height=7, 
           options=list(onclick="function(id){ alert('You clicked on: ' + id); }"))
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
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold", color = "black"),
        axis.ticks.y = element_blank(),  # Remove radial Y-axis ticks
        axis.text.y = element_blank(),  # Remove radial Y-axis text
        panel.grid.major = element_line(color = "grey80"),  # Add light grey radial grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),  # Remove border
        legend.position = "none"  # Hide legend
        
      ) +
      labs(y = NULL, x = NULL, title = paste("Specific Area Average Number of Jobs in", input$selected_LGA, "for 2019-20"))
    
    girafe(ggobj = p, options = list(
      height_svg = 5,
      width_svg  = 7,
      onclick = "function(id){
               Shiny.setInputValue('clicked_area', id);
             }"
    ))
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
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_blank(), # Hide y-axis text
        axis.ticks.y = element_blank(), # Hide y-axis ticks
        panel.grid.major = element_blank(), # Remove major grid
        panel.grid.minor = element_blank(), # Remove minor grid
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color="black") # Add border to plot
      ) +
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
      geom_bar_interactive(stat = "identity") +
      coord_flip() +
      labs(y = "Total Number of Jobs", x = "LGA", title = "Number of Jobs in Selected LGAs in 2019-20") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),  # Remove major grid
        panel.grid.minor = element_blank(),  # Remove minor grid
        panel.background = element_blank(),  # Remove background
        panel.border = element_rect(fill=NA, color="black")  # Add border
      )
    
    girafe(ggobj = p, width = 9, height = 7)
  })
  
  
}





#############
# RUN SHINY #
#############

shinyApp(ui, server)
