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
                      
             ),
             tabPanel("Building approved by State",
                      
             ),
             tabPanel("Road infrastructure development",
                      
             ),
       tabPanel("Population",
                  
                ),
       tabPanel(tags$div("LGA: Local Government Area"), 
                absolutePanel(
                  top=50, left = 40,
                  selectInput(inputId = "selectedYear", label = "Select Year", 
                              choices = c("2016-17" = "X2016.17", 
                                          "2017-18" = "X2017.18", 
                                          "2018-19" = "X2018.19", 
                                          "2019-20" = "X2019.20"),
                              selected = "X2019.20")
              
                  
                ),
                absolutePanel(
                  top=30, left = 400,
                  selectInput("selected_area", "More Specific Area: Statistic Area Level 2", choices = sort(unique(industry_data$Area[industry_data$Lga == "Melbourne"])))
                ),
                absolutePanel(
                  top=150, left = 40, width =600, height= 400,
                  girafeOutput("rankedLgaChart")
                ),
                absolutePanel(
                  top=100, left = 700, width =400, height= 400,
                  girafeOutput("interactiveBarChart")
                ),
                absolutePanel(
                  top=400, left = 40, width =600, height= 400,
                  girafeOutput("job_pie_chart")
                ),
                absolutePanel(
                  top=400, left = 700, width =600, height= 400,
                  girafeOutput("combinedLineChart")
                  
                ),
               )
       
       
       )
)

  

################
# SHINY SERVER #
################

server <- function(input, output) {
 
#################################################################################
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
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
      facet_wrap(~ Source, scales = "free_y") +
      labs(title = "Average Jobs and Salary (2015-2019)",
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
    
    girafe(ggobj = p_combined, width = 9, height = 7)
  })
  
  
  library(RColorBrewer)
  
  set3_colors <- brewer.pal(12, "Set3")
  pastel1_colors <- brewer.pal(9, "Pastel1")
  pastel2_colors <- brewer.pal(8, "Pastel2")
  combined_palette <- c(set3_colors, pastel1_colors, pastel2_colors[1:7])  # We need 7 colors from Pastel2 to get a total of 19
  
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
    
    girafe(ggobj = p, width = 10, height = 10)
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
