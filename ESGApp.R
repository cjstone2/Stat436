library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(RColorBrewer)
library(viridisLite)
library(tidyr)


# Load datasets
data1 <- read.csv("https://raw.githubusercontent.com/cjstone2/Stat436/refs/heads/main/ESGdata.csv")
data2 <- read.csv("https://raw.githubusercontent.com/jasonjclark/stat436deadline2/refs/heads/main/publiccompanies.csv") %>%
  mutate(industry = case_when(
    industry == "Aerospace & Defense" ~ "Aerospace and Defense",
    industry == "Metals & Mining" ~ "Metals and Mining",
    TRUE ~ industry
  ))
data3 <- read_csv("https://raw.githubusercontent.com/ManaswiKolani/Stat436ProjectMilestone2/refs/heads/main/data.csv")
data4 <- read.csv("https://raw.githubusercontent.com/ekassz/ESGProject/refs/heads/main/data/data.csv")

data4$esg_category <- case_when(
  data4$environment_grade %in% c("AAA", "AA", "A") ~ "High",
  data4$environment_grade %in% c("BBB", "BB", "B") ~ "Medium",
  data4$environment_grade %in% c("CCC", "CC", "C") ~ "Low",
  TRUE ~ "Unknown"
)

# UI
ui <- fluidPage(
  titlePanel("ESG Insights"),
  tabsetPanel(
    # Tab 1: Radar Chart Comparison
    tabPanel("Radar Chart Comparison",
             sidebarLayout(
               sidebarPanel(
                 helpText("Compare companies' ESG performance using a radar chart."),
                 selectInput("company1", "Select First Company:", choices = unique(data1$name)),
                 selectInput("company2", "Select Second Company:", choices = unique(data1$name)),
                 helpText("Each axis represents a different ESG dimension: Environmental, Social, and Governance.")
               ),
               mainPanel(plotlyOutput("esgRadar"))
             )),
    
    # Tab 2: Boxplots by Industry
    tabPanel("Boxplots by Industry",
             sidebarLayout(
               sidebarPanel(
                 selectInput("esg_category", "Select ESG Category:",
                             choices = c("Environmental" = "environment_score",
                                         "Social" = "social_score",
                                         "Governance" = "governance_score",
                                         "Total ESG" = "total_score")),
                 selectizeInput("industries", "Select Industries:",
                                choices = sort(unique(data2$industry)),
                                selected = sort(unique(data2$industry))[1:5], multiple = TRUE),
                 helpText("Hover over points to see company details.")
               ),
               mainPanel(plotlyOutput("boxplot", height = "700px"))
             )),
    
    # Tab 3: ESG Heatmap and Summary
    tabPanel("ESG Heatmap and Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("industry_input", "Select Industry:", choices = unique(data3$industry)),
                 helpText("Explore ESG scores and grades within a selected industry."),
                 verbatimTextOutput("statisticsText")
               ),
               mainPanel(
                 plotOutput("industrySummaryPlot"),
                 plotOutput("esgHeatmapPlot"),
                 tableOutput("comparisonTable")
               )
             )),
    
    # Tab 4: ESG Grade Distribution
    tabPanel("ESG Grade Distribution",
             sidebarLayout(
               sidebarPanel(
                 selectInput("env_grade", "Environmental Grade:", choices = c("All", "High", "Medium", "Low")),
                 selectInput("soc_grade", "Social Grade:", choices = c("All", "High", "Medium", "Low")),
                 selectInput("gov_grade", "Governance Grade:", choices = c("All", "High", "Medium", "Low")),
                 selectInput("score_type", "Score Type:", 
                             choices = c("Environmental Score" = "environment_score",
                                         "Social Score" = "social_score",
                                         "Governance Score" = "governance_score")),
                 sliderInput("bin_width", "Bin Width:", min = 5, max = 50, value = 10)
               ),
               mainPanel(
                 plotlyOutput("esgBar"),
                 DT::DTOutput("filteredData")
               )
             ))
  )
)

# Server
server <- function(input, output, session) {
  
  # Tab 1: Radar Chart Comparison
  output$esgRadar <- renderPlotly({
    company1_scores <- data1 %>%
      filter(name == input$company1) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    company2_scores <- data1 %>%
      filter(name == input$company2) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    metrics <- c("Environmental Score", "Social Score", "Governance Score")
    
    plot_ly(
      type = 'scatterpolar',
      mode = 'lines'
    ) %>%
      add_trace(
        r = company1_scores,
        theta = metrics,
        fill = 'toself',
        name = input$company1,
        text = paste0(metrics, ": ", company1_scores),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        r = company2_scores,
        theta = metrics,
        fill = 'toself',
        name = input$company2,
        text = paste0(metrics, ": ", company2_scores),
        hoverinfo = 'text'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(data1[c("environment_score", "social_score", "governance_score")]))
          )
        ),
        title = "ESG Performance Comparison"
      )
  })
  
  # Tab 2: Boxplots by Industry
  output$boxplot <- renderPlotly({
    filtered_data <- data2 %>%
      filter(industry %in% input$industries)
    
    esg_label <- switch(input$esg_category,
                        "environment_score" = "Environmental Score",
                        "social_score" = "Social Score",
                        "governance_score" = "Governance Score",
                        "total_score" = "Total ESG Score")
    
    ggplotly(
      ggplot(filtered_data, aes(x = industry, y = .data[[input$esg_category]], fill = industry)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(aes(text = paste("Company:", name)), width = 0.2, size = 1) +
        labs(title = esg_label, x = "Industry", y = esg_label) +
        theme_minimal()
    )
  })
  
  # Tab 3: ESG Heatmap and Summary
  output$statisticsText <- renderPrint({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input)
    
    highest <- filtered_data[which.max(filtered_data$total_score), ]
    lowest <- filtered_data[which.min(filtered_data$total_score), ]
    
    cat("Highest ESG Score:\n")
    cat("Company:", highest$name, "| Total Score:", highest$total_score, "\n\n")
    
    cat("Lowest ESG Score:\n")
    cat("Company:", lowest$name, "| Total Score:", lowest$total_score, "\n")
  })
  
  output$industrySummaryPlot <- renderPlot({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input)
    
    ggplot(filtered_data, aes(x = reorder(name, -total_score), y = total_score, fill = total_grade)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("A" = "green", "BBB" = "yellow", "BB" = "orange", "B" = "red")) +
      labs(title = paste("ESG Scores in", input$industry_input), x = "Company", y = "Total Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$esgHeatmapPlot <- renderPlot({
    filtered_data <- data3 %>%
      filter(industry == input$industry_input) %>%
      pivot_longer(cols = c(environment_score, social_score, governance_score), 
                   names_to = "criteria", values_to = "score")
    
    ggplot(filtered_data, aes(x = name, y = criteria, fill = score)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = paste("ESG Heatmap for", input$industry_input), x = "Company", y = "Criteria") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$comparisonTable <- renderTable({
    data3 %>%
      filter(industry == input$industry_input) %>%
      select(name, total_score, total_grade)
  })
  
  # Tab 4: ESG Grade Distribution
  output$esgBar <- renderPlotly({
    filtered_data <- data4 %>%
      filter(esg_category == input$env_grade | input$env_grade == "All") %>%
      mutate(tooltip = paste("Company:", name, "<br>Score:", get(input$score_type)))
    
    ggplotly(
      ggplot(filtered_data, aes_string(x = input$score_type, text = "tooltip")) +
        geom_histogram(binwidth = input$bin_width, fill = "blue", color = "darkblue", alpha = 0.7) +
        geom_vline(aes(xintercept = mean(get(input$score_type), na.rm = TRUE)), linetype = "dashed", color = "red") +
        labs(title = paste("Distribution of", gsub("_", " ", input$score_type)),
             x = gsub("_", " ", input$score_type), y = "Frequency") +
        theme_minimal()
    )
  })
  
  output$filteredData <- renderDT({
    data4 %>%
      filter(esg_category == input$env_grade | input$env_grade == "All") %>%
      select(name, environment_score, social_score, governance_score, total_score)
  })
}

# Run App
shinyApp(ui, server)




