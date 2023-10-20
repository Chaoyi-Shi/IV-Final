library("shiny")
library("leaflet")
library('dplyr')
library("ggplot2")
library("ggiraph")
library("reshape2")
library(RColorBrewer)
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
  tags$div("LGA: Local Government Area"),  # Add this line
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
    )
  )
)





################
# SHINY SERVER #
################

server <- function(input, output,session) {

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
# RUN SHINY #
#############

shinyApp(ui, server)
