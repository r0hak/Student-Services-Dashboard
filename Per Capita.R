library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(forcats)

# Path to Excel file defined
excel_path <- "/Users/r0hak/Downloads/4th Year/CS Final Year Project/FYP Data File.xlsx"

# UI Defined
ui <- dashboardPage(
  dashboardHeader(
    title = "Third Level Student Services", 
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("HEA Population", tabName = "hea_pop", startExpanded = TRUE, icon = icon("university"),
             menuSubItem("Overall Population", tabName = "overall_pop"),
             menuSubItem("Institute Breakdown", tabName = "breakdown")
      ),
      menuItem("Institute Data", tabName = "per_capita", startExpanded = TRUE, icon = icon("person"),
             menuSubItem("Counselling", tabName = "counselling"),
             menuSubItem("Health", tabName = "health"),
             menuSubItem("Learning Support", tabName = "learning_support"),
             menuSubItem("Careers", tabName = "careers")
      ),
      menuItem("HEA vs Student Survey", tabName = "hea_vs_student_survey", startExpanded = TRUE, icon = icon("area-chart"),
             menuSubItem("Gender", tabName = "gender"),
             menuSubItem("Age", tabName = "age"),
             menuSubItem("Study Mode", tabName = "study_mode"),
             menuSubItem("Institute Type", tabName = "institute_type")
      ),
      menuItem("Student Survey", tabName = "student_survey", startExpanded = TRUE, icon = icon("pie-chart"),
             menuSubItem("Careers Use", tabName = "careers_use"),
             menuSubItem("Support Staff Quality", tabName = "support_staff_quality"),
             menuSubItem("Learning Support Use", tabName = "learning_support_use"),
             menuSubItem("Well-being Emphasis", tabName = "well_being_emphasis")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
        fluidRow(
          box(title = tags$h3("Welcome to the Third Level Student Services Dashboard"), status = "primary",
              solidHeader = TRUE, width = 12,
              tags$ul(
                tags$li("This dashboard provides an interactive visualisation of data from the StudentSurvey, HEA, 
                  and various institutes across the country."),
                tags$li("Navigate through the tabs to explore different aspects of student services.")
              )
          )
        ), 
        fluidRow(
          column(width = 6,
                 box(title = tags$h4("HEA Population", icon = icon("users")), status = "info", solidHeader = TRUE, width = 12, height = "250px",
                     tags$ul(
                       tags$li("View the change in population of students enrolled in third level institutes in Ireland from the 2016/17 to the 2022/23 academic year."),
                       tags$li("See the overall figures as well as the institute breakdown."),
                       tags$li("Data retrieved from the official HEA website.", 
                               tags$a(href = "https://hea.ie/statistics/data-for-download-and-visualisations/access-our-data/access-our-data-students/", 
                                      "Access HEA Data")
                       )
                     )
                 )
          ),
          column(width = 6,
                 box(title = tags$h4("Institute Data", icon = icon("university")), status = "info", solidHeader = TRUE, width = 12, height = "250px",
                     tags$ul(
                       tags$li("Visualise usage levels of student services by institute."),
                       tags$li("Includes counselling, health, learning support, and careers service."),
                       tags$li("Data collected from online reports and direct institute contact.")
                     )
                 )
          )
        ), 
        fluidRow(
          column(width = 6,
                 box(title = tags$h4("HEA vs Student Survey", icon = icon("chart-bar")), status = "info", solidHeader = TRUE, width = 12, height = "250px",
                     tags$ul(
                       tags$li("Compare HEA's and StudentSurvey's demographic data."),
                       tags$li("Visualise demographic representativeness of the survey compared to the population.")
                     )
                 )
          ),
          column(width = 6,
                 box(title = tags$h4("StudentSurvey Breakdown", icon = icon("poll")), status = "info", solidHeader = TRUE, width = 12, height = "250px",
                     tags$ul(
                       tags$li("Explore answers to Student Services-related questions in the StudentSurvey over the years.")
                     )
                 )
          )
        )
      ),
      tabItem(tabName = "breakdown",
        h3("Population Over Years", align = "center"),
        plotlyOutput("populationPlot")
      ),
      tabItem(tabName = "overall_pop",
              h3("Combined Population of HEIs", align = "center"),
              plotlyOutput("overallPlot")
      ),
      tabItem(tabName = "counselling",
        h3("Counselling Usage Rates", align = "center"),
        plotlyOutput("counsellingPlot")
      ),
      tabItem(tabName = "health",
        h3("Health Usage Rates", align = "center"),
        plotlyOutput("healthPlot")
      ),
      tabItem(tabName = "learning_support",
        h3("Learning Support Usage Rates", align = "center"),
        plotlyOutput("lsPlot")
      ), 
      tabItem(tabName = "careers",
        h3("Careers Guidance Usage Rates", align = "center"),
        plotlyOutput("careersPlot")
      ), 
      tabItem(tabName = "gender", 
        h3("StudentSurvey Gender Breakdown", align = "center"), 
        plotlyOutput("genderPlot"),
        tags$div(
          style = "margin-top: 20px; padding: 15px; background-color: #f7f7f9; border: 1px solid #e1e1e8; border-radius: 5px; text-align: center;",
          tags$h4("Chi-square Test Results", style = "margin-bottom: 10px; color: #337ab7;"),
          uiOutput("genderPlotText")
        )
      ),
      tabItem(tabName = "study_mode", 
        h3("StudentSurvey Study Mode Breakdown", align = "center"), 
        plotlyOutput("studyModePlot"),
        tags$div(
          style = "margin-top: 20px; padding: 15px; background-color: #f7f7f9; border: 1px solid #e1e1e8; border-radius: 5px; text-align: center;",
          tags$h4("Chi-square Test Results", style = "margin-bottom: 10px; color: #337ab7;"),
          uiOutput("studymodePlotText")
        )
      ),
      tabItem(tabName = "age", 
        h3("StudentSurvey Age Breakdown", align = "center"), 
        plotlyOutput("agePlot"),
        tags$div(
          style = "margin-top: 20px; padding: 15px; background-color: #f7f7f9; border: 1px solid #e1e1e8; border-radius: 5px; text-align: center;",
          tags$h4("Chi-square Test Results", style = "margin-bottom: 10px; color: #337ab7;"),
          uiOutput("agePlotText")
        )
      ), 
      tabItem(tabName = "institute_type", 
        h3("StudentSurvey Institute Type Breakdown", align = "center"), 
        plotlyOutput("instituteTypePlot"),
        tags$div(
          style = "margin-top: 20px; padding: 15px; background-color: #f7f7f9; border: 1px solid #e1e1e8; border-radius: 5px; text-align: center;",
          tags$h4("Chi-square Test Results", style = "margin-bottom: 10px; color: #337ab7;"),
          uiOutput("institutetypePlotText")
        )
      ),
      tabItem(tabName = "careers_use", 
              h3("StudentSurvey Careers Discussion", align = "center"), 
              plotlyOutput("careersUsePlot")
      ),
      tabItem(tabName = "support_staff_quality", 
              h3("StudentSurvey Support Staff Quality", align = "center"), 
              plotlyOutput("supportStaffPlot")
      ),
      tabItem(tabName = "learning_support_use", 
              h3("StudentSurvey Learning Support Emphasis", align = "center"), 
              plotlyOutput("lsUsePlot")
      ),
      tabItem(tabName = "well_being_emphasis", 
              h3("StudentSurvey Well-Being Emphasis", align = "center"), 
              plotlyOutput("WBEmpPlot")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read the data from the Excel file and which sheet is relevant
  population_data <- read_excel(excel_path, sheet = "HEA Pop")
  overallpop_data <- read_excel(excel_path, sheet = "Overall Pop")
  counsellingPerCap_data <- read_excel(excel_path, sheet = "Counselling Per Capita")
  healthPerCap_data <- read_excel(excel_path, sheet = "Health Per Capita")
  lsPerCap_data <- read_excel(excel_path, sheet = "Learning Support Per Capita")
  careersPerCap_data <- read_excel(excel_path, sheet = "Careers Per Capita")
  SSGender_data <- read_excel(excel_path, sheet = "SS Gender")
  SSAge_data <- read_excel(excel_path, sheet = "SS Age Group")
  SSStudyMode_data <- read_excel(excel_path, sheet = "SS Study Mode")
  SSInstituteType_data <- read_excel(excel_path, sheet = "SS Institute Type")
  SSCareersUse_data <- read_excel(excel_path, sheet = "SS Careers")
  SSStaffQ_data <- read_excel(excel_path, sheet = "SS StaffQ")
  SSLSEm_data <- read_excel(excel_path, sheet = "SS LSEm")
  SSWellEm <- read_excel(excel_path, sheet = "SS WellE")

  # Reactive expression for filtered and transformed data
  data_long <- reactive({
    selected_institutes <- input$selected_institutes
    if (is.null(selected_institutes) || length(selected_institutes) == 0) {
      selected_institutes <- unique(population_data$Institute)
    }
    
    # Filter data based on selected institutes
    filtered_data <- population_data %>%
      filter(Institute %in% selected_institutes)
    
    # Convert to long format for plotting
    pivot_longer(filtered_data, cols = -Institute, names_to = "Year", values_to = "Population")
  })
  
  
  output$counsellingPlot <- renderPlotly({
    counselling_data_long <- counsellingPerCap_data %>%
      pivot_longer(
        cols = -Counselling,  
        names_to = "Academic_Year",
        values_to = "Per_Capita_Counselling",
        values_drop_na = TRUE
      ) 
    
    # Create the plot
    p <- ggplot(counselling_data_long, aes(x = Academic_Year, y = Per_Capita_Counselling, group = Counselling,
                                       color = Counselling, text = paste("Institute:", Counselling, "<br>Year:", 
                                                                         Academic_Year, "<br>Counselling Usage Rate:", 
                                                                         sprintf("%.2f", Per_Capita_Counselling)))) +
      geom_line() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
      ) +
      labs(title = "Counselling Services Usage Rate", x = "Academic Year", y = "Usage Rate") +
      theme(legend.position = "bottom")
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = "text")
    ggplotly_plot <- ggplotly_plot %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian'),
             modeBarButtonsToAdd = list('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'))
    
    ggplotly_plot
  })
  
  output$lsPlot <- renderPlotly({
    ls_data_long <- lsPerCap_data %>%
      pivot_longer(
        cols = -Learning, 
        names_to = "Academic_Year",
        values_to = "Per_Capita_ls",
        values_drop_na = TRUE
      ) 
    
    # Create the plot
    p <- ggplot(ls_data_long, aes(x = Academic_Year, y = Per_Capita_ls, group = Learning,
                                       color = Learning, text = paste("Institute:",
                                                                      Learning, "<br>Year:", Academic_Year, "<br>Learning Support Usage Rate:", sprintf("%.2f",Per_Capita_ls)))) +
      geom_line() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
      ) +
      labs(title = "Learning Support Usage Rate", x = "Academic Year", y = "Usage Rate") +
      theme(legend.position = "bottom")
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = "text")
    ggplotly_plot <- ggplotly_plot %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian'),
             modeBarButtonsToAdd = list('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'))
    
    ggplotly_plot
  })
  
  output$studyModePlot <- renderPlotly({
    # Create the plot
    studyMode_data_long <- SSStudyMode_data %>%
      pivot_longer(
        cols = -c(Year, Set),  # Selects all columns except 'Year' and 'Set' for pivoting
        names_to = "Study",
        values_to = "Percentage"
      )
    p <- ggplot(studyMode_data_long, aes(x = interaction(Set, Year, sep = " "), y = Percentage, fill = Study,
                                         text = paste("Percentage:", round(Percentage * 100, 2), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Full Time" = "#1E88E5", "Part Time" = "#D81B60", "Other" = "#FFC107")) +
      theme_minimal() +
      labs(title = "Study Mode Distribution by Year and Set",
           x = "Dataset",
           y = "Proportion",
           fill = "Study Mode") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = c("text", "fill")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Dataset'),
        yaxis = list(title = 'Proportion')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # Enable the display mode bar for more interactivity
    
    # Add event handlers if needed
    ggplotly_plot <- ggplotly_plot %>%
      event_register("plotly_hover") %>%
      event_register("plotly_click")
    
    ggplotly_plot
  })
  
  output$instituteTypePlot <- renderPlotly({
    # Create the plot
    instituteType_data_long <- SSInstituteType_data %>%
      pivot_longer(
        cols = -c(Year, Set),  # Selects all columns except 'Year' and 'Set' for pivoting
        names_to = "Institute",
        values_to = "Percentage"
      )
    p <- ggplot(instituteType_data_long, aes(x = interaction(Set, Year, sep = " "), y = Percentage, fill = Institute,
                                             text = paste("Percentage:", round(Percentage * 100, 2), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("University" = "#1E88E5", "TU" = "#D81B60", "Other" = "#FFC107")) +
      theme_minimal() +
      labs(title = "Institute Type Distribution by Year and Set",
           x = "Dataset",
           y = "Proportion",
           fill = "Institute Type") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = c("text", "fill")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Dataset'),
        yaxis = list(title = 'Proportion')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # Enable the display mode bar for more interactivity
    
    # event handlers if needed
    ggplotly_plot <- ggplotly_plot %>%
      event_register("plotly_hover") %>%
      event_register("plotly_click")
    
    ggplotly_plot
  })
  
  output$genderPlot <- renderPlotly({
    # Create the plot
    gender_data_long <- SSGender_data %>%
      pivot_longer(
        cols = -c(Year, Set),  # Selects all columns except 'Year' and 'Set' for pivoting
        names_to = "Gender",
        values_to = "Percentage" 
      ) 
    p <- ggplot(gender_data_long, aes(x = interaction(Set, Year, sep = " "), y = Percentage, fill = Gender,
                                      text = paste("Percentage:", round(Percentage * 100, 2), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Male" = "#1E88E5", "Female" = "#D81B60", "Undeclared" = "#FFC107", "Non-Binary" = "#004D40")) +
      theme_minimal() +
      labs(title = "Gender Distribution by Year and Set",
           x = "Dataset",
           y = "Proportion",
           fill = "Gender") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = c("text", "fill")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Dataset'),
        yaxis = list(title = 'Proportion')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # Enable the display mode bar for more interactivity
    
    # event handlers if needed
    ggplotly_plot <- ggplotly_plot %>%
      event_register("plotly_hover") %>%
      event_register("plotly_click")
    
    ggplotly_plot
  })
  
  output$genderPlotText <- renderUI({
    HTML("2022: Chi-Square Test = 1.884e-148 <br> 2021: Chi-Square Test = 3.4853e-160
         <br> 2020: Chi-Square Test = 6.1436e-110 <br> 2019: Chi-Square Test = 5.8638e-109 
         <br> 2018: Chi-Square Test = 8.6984e-134")
  })
  
  output$studymodePlotText <- renderUI({
    HTML("2022: Chi-Square Test = p < 0.0001 <br> 2021: Chi-Square Test = p < 0.0001
         <br> 2020: Chi-Square Test = p < 0.0001 <br> 2019: Chi-Square Test = p < 0.0001 
         <br> 2018: Chi-Square Test = p < 0.0001")
  })
  
  output$institutetypePlotText <- renderUI({
    HTML("2022: Chi-Square Test = 1.1496e-190 <br> 2021: Chi-Square Test = 8.4885e-184 
         <br> 2020: Chi-Square Test = 1.5306e-133 <br> 2019: Chi-Square Test = 2.8244e-258 
         <br> 2018: Chi-Square Test = 3.5173e-115")
  })
  
  output$agePlot <- renderPlotly({
    # Create the plot
    gender_data_long <- SSAge_data %>%
      pivot_longer(
        cols = -c(Year, Set),  # Selects all columns except 'Year' and 'Set' for pivoting
        names_to = "Age",
        values_to = "Percentage"
      )
    p <- ggplot(gender_data_long, aes(x = interaction(Set, Year, sep = " "), y = Percentage, fill = Age,
                                      text = paste("Percentage:", round(Percentage * 100, 2), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("< 23" = "#1E88E5", "> 24" = "#D81B60", "Unknown" = "#FFC107")) +
      theme_minimal() +
      labs(title = "Age Distribution by Year and Set",
           x = "Dataset",
           y = "Proportion",
           fill = "Age") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = c("text", "fill")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Dataset'),
        yaxis = list(title = 'Proportion')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # Enable the display mode bar for more interactivity
    
    # event handlers if needed
    ggplotly_plot <- ggplotly_plot %>%
      event_register("plotly_hover") %>%
      event_register("plotly_click")
    
    ggplotly_plot
  })
  
  output$agePlotText <- renderUI({
    HTML("2022: Chi-Square Test = 0.058031394 <br> 2021: Chi-Square Test = 6.2827e-14 
         <br> 2020: Chi-Square Test = 8.15739e-49 <br> 2019: Chi-Square Test = 1.89178e-20 
         <br> 2018: Chi-Square Test = 7.38693e-08")
  })
  
  output$careersPlot <- renderPlotly({
    careers_data_long <- careersPerCap_data %>%
      pivot_longer(
        cols = -Careers,  # Selects all columns except 'Health' for pivoting
        names_to = "Academic_Year",
        values_to = "Per_Capita_Careers",
        values_drop_na = TRUE
      ) 
    
    # Create the plot
    p <- ggplot(careers_data_long, aes(x = Academic_Year, y = Per_Capita_Careers, group = Careers,
                                      color = Careers, text = paste("Institute:", Careers, "<br>Year:", Academic_Year, "<br>Careers Service Usage:", sprintf("%.2f",Per_Capita_Careers)))) +
      geom_line() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
      ) +
      labs(title = "Career Services Usage", x = "Academic Year", y = "Usage Rate") +
      theme(legend.position = "bottom")
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = "text")
    ggplotly_plot <- ggplotly_plot %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian'),
             modeBarButtonsToAdd = list('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'))
    
    ggplotly_plot
  })
  
  output$healthPlot <- renderPlotly({
    # Transform the health data from wide to long format
    health_data_long <- healthPerCap_data %>%
      pivot_longer(
        cols = -Health,  # Selects all columns except 'Health' for pivoting
        names_to = "Academic_Year",
        values_to = "Per_Capita_Health",
        values_drop_na = TRUE
      ) 
    
    # Create the plot
    p <- ggplot(health_data_long, aes(x = Academic_Year, y = Per_Capita_Health, group = Health,
                                      color = Health, text = paste("Institute:", Health, "<br>Year:", Academic_Year, "<br>Health Service Usage:", sprintf("%.2f",Per_Capita_Health)))) +
      geom_line() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
      ) +
      labs(title = "Health Services Usage Rate", x = "Academic Year", y = "Usage Rate") +
      theme(legend.position = "bottom")
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = "text")
    ggplotly_plot <- ggplotly_plot %>%
      config(displayModeBar = TRUE, displaylogo = FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian'),
             modeBarButtonsToAdd = list('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'))
    
    ggplotly_plot
  })
  
  
  # Generate interactive plot with plotly
  output$populationPlot <- renderPlotly({
    # Use the reactive data
    plot_data <- data_long()
    
    p <- ggplot(plot_data, aes(x = Year, y = Population, group = Institute,
                               color = Institute, text = paste("Institute:", Institute, "<br>Year:", Year, "<br>Population:", Population))) +
      geom_line() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank()
      ) +
      labs(title = "Population of Selected Institutes", x = "Year", y = "Population") +
      theme(legend.position = "bottom")
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = "text")
    ggplotly_plot <- ggplotly_plot %>% 
      config(displayModeBar = TRUE, displaylogo = FALSE, 
             modeBarButtonsToRemove = list('sendDataToCloud', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian'), 
             modeBarButtonsToAdd = list('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'resetScale2d'))
    
    ggplotly_plot
  })
  
  output$overallPlot <- renderPlotly({
    p <- ggplot(overallpop_data, aes(x = Year, y = Population)) +
      geom_bar(stat = "identity", fill = "#1E88E5", width = 0.5) +
      theme_minimal() +
      labs(title = "HEA Population Over the Years",
           x = "Year",
           y = "Population") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    # Convert the ggplot object to a Plotly object for interactivity
    ggplotly_plot <- ggplotly(p, tooltip = c("x", "y")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Year'),
        yaxis = list(title = 'Population (000s)')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE) # The display mode bar can be enabled for more interactivity
    
    ggplotly_plot
  })
  
  output$careersUsePlot <- renderPlotly({
    careersUse_data_long <- SSCareersUse_data %>%
      pivot_longer(
        cols = -c(Years),  
        names_to = "ResponseCategory",
        values_to = "Count"
      )
    
    # Convert ResponseCategory to a factor with levels in Likert order
    careersUse_data_long$ResponseCategory <- factor(careersUse_data_long$ResponseCategory,
                                                    levels = c("No Answer", "Never", "Sometimes", "Often", "Very Often"))
    
    total_counts <- careersUse_data_long %>% 
      group_by(Years) %>% 
      summarise(Total = sum(Count))
    
    careersUse_data_long <- careersUse_data_long %>% 
      left_join(total_counts, by = "Years") %>% 
      mutate(Percentage = Count / Total * 100)
    
    p <- ggplot(careersUse_data_long, aes(x = as.factor(Years), y = Count, fill = ResponseCategory,
                                          text = paste(ResponseCategory, ":", round(Percentage, 2), "%"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Very Often" = "#1E88E5", "Often" = "#ff7f0e", "Sometimes" = "#004D40", "Never" = "#D81B60", "No Answer" = "#7f7f7f")) +
      theme_minimal() +
      labs(title = "Careers Discussion Frequency",
           x = "Year",
           y = "Count",
           fill = "Response") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly_plot <- ggplotly(p, tooltip = c("y", "text")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Count'),
        yaxis = list(title = 'Year')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    ggplotly_plot
  })
  
  output$supportStaffPlot <- renderPlotly({
    supportStaff_data_long <- SSStaffQ_data %>%
      pivot_longer(
        cols = -c(Years),  
        names_to = "ResponseCategory",
        values_to = "Count"
      )
    total_counts <- supportStaff_data_long %>% 
      group_by(Years) %>% 
      summarise(Total = sum(Count))
    
    # Join with original data and calculate percentages
    supportStaff_data_long <- supportStaff_data_long %>% 
      left_join(total_counts, by = "Years") %>% 
      mutate(Percentage = Count / Total * 100)
    
    # Create the plot
    p <- ggplot(supportStaff_data_long, aes(x = as.factor(Years), y = Count, fill = ResponseCategory,
                                            text = paste(ResponseCategory, ":", round(Percentage, 2), "%"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("1 (Poor)" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73", "4" = "#F0E442", "5" = "#0072B2", "6" = "#D55E00", "7 (Excellent)" = "#CC79A7", "No Answer" = "#7f7f7f")) +
      theme_minimal() +
      labs(title = "Quality of Support Staff",
           x = "Year",
           y = "Count",
           fill = "Response") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert the ggplot object to a plotly object
    ggplotly_plot <- ggplotly(p, tooltip = c("y", "text")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Count'),
        yaxis = list(title = 'Year')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    ggplotly_plot
  })
  
  output$lsUsePlot <- renderPlotly({
    lsUse_data_long <- SSLSEm_data %>%
      pivot_longer(
        cols = -c(Years),  
        names_to = "ResponseCategory",
        values_to = "Count"
      )
    
    # Convert ResponseCategory to a factor with levels in the specified order
    lsUse_data_long$ResponseCategory <- factor(lsUse_data_long$ResponseCategory,
                                               levels = c("No Answer", "Very Little", "Some", "Quite a bit", "Very Much"))
    
    total_counts <- lsUse_data_long %>% 
      group_by(Years) %>% 
      summarise(Total = sum(Count))
    
    lsUse_data_long <- lsUse_data_long %>% 
      left_join(total_counts, by = "Years") %>% 
      mutate(Percentage = Count / Total * 100)
    
    p <- ggplot(lsUse_data_long, aes(x = as.factor(Years), y = Count, fill = ResponseCategory,
                                     text = paste(ResponseCategory, ":", round(Percentage, 2), "%"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Very Little" = "#E69F00", "Some" = "#56B4E9", "Quite a bit" = "#009E73", "Very Much" = "#F0E442", "No Answer" = "#CC79A7")) +
      theme_minimal() +
      labs(title = "Institute Emphasis on Learning Support",
           x = "Year",
           y = "Count",
           fill = "Response") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly_plot <- ggplotly(p, tooltip = c("y", "text")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Count'),
        yaxis = list(title = 'Year')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    ggplotly_plot
  })
  
  output$WBEmpPlot <- renderPlotly({
    WellbeingEm_data_long <- SSWellEm %>%
      pivot_longer(
        cols = -c(Years),  
        names_to = "ResponseCategory",
        values_to = "Count"
      )
    
    # Convert ResponseCategory to a factor with levels in the specified order
    WellbeingEm_data_long$ResponseCategory <- factor(WellbeingEm_data_long$ResponseCategory,
                                                     levels = c("No Answer", "Very Little", "Some", "Quite a bit", "Very Much"))
    
    total_counts <- WellbeingEm_data_long %>% 
      group_by(Years) %>% 
      summarise(Total = sum(Count))
    
    WellbeingEm_data_long <- WellbeingEm_data_long %>% 
      left_join(total_counts, by = "Years") %>% 
      mutate(Percentage = Count / Total * 100)
    
    p <- ggplot(WellbeingEm_data_long, aes(x = as.factor(Years), y = Count, fill = ResponseCategory,
                                           text = paste(ResponseCategory, ":", round(Percentage, 2), "%"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Very Little" = "#E69F00", "Some" = "#56B4E9", "Quite a bit" = "#009E73", "Very Much" = "#F0E442", "No Answer" = "#CC79A7")) +
      theme_minimal() +
      labs(title = "Institute Emphasis on Well-Being",
           x = "Year",
           y = "Count",
           fill = "Response") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly_plot <- ggplotly(p, tooltip = c("y", "text")) %>% 
      layout(
        hovermode = 'closest',
        xaxis = list(title = 'Count'),
        yaxis = list(title = 'Year')
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
    
    ggplotly_plot
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "counselling") {
      output$currentPlot <- output$counsellingPlot
    } else if (input$tabs == "health") {
      output$currentPlot <- output$healthPlot
    } else if (input$tabs == "learning_support") {
      output$currentPlot <- output$lsPlot
    } else if (input$tabs == "careers") {
      output$currentPlot <- output$careersPlot
    } else if (input$tabs == "gender") {
      output$currentPlot <- output$genderPlot
    } else if (input$tabs == "age") {
      output$currentPlot <- output$agePlot
    } else if (input$tabs == "study_mode") {
      output$currentPlot <- output$studyModePlot
    } else if (input$tabs == "institute_type") {
      output$currentPlot <- output$instituteTypePlot
    } else if (input$tabs == "careers_use") {
      output$currentPlot <- output$careersUsePlot
    } else if (input$tabs == "support_staff_quality") {
      output$currentPlot <- output$supportStaffPlot
    } else if (input$tabs == "learning_support_use") {
      output$currentPlot <- output$lsUsePlot
    } else if (input$tabs == "well_being_emphasis") {
      output$currentPlot <- output$WBEmpPlot
    } else if (input$tabs == "hea_pop") {
      output$currentPlot <- output$populationPlot
    }
  })
}

# Run the application 
shinyApp(ui, server)


