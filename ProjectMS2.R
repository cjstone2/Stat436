library(shiny)
library(plotly)
library(dplyr)

data <- read.csv("ESGdata.csv")

ui <- fluidPage(
  titlePanel("Public Company ESG Score Radar Chart Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This chart will help you understand how companies perform across different dimensions of ESG (Environmental, Social, and Governance). Each axis represents a different ESG dimension. The larger the area within the chart, the higher the company's scores across these dimensions."),
      
      helpText(
        strong("What do Environmental, Social, and Governance mean?")
      ),
      helpText("1. ", strong("Environmental (E)"), ": This measures a company's impact on the environment, including factors like carbon emissions, waste management, and resource conservation."),
      helpText("2. ", strong("Social (S)"), ": This evaluates a company's relationships with employees, suppliers, customers, and communities."),
      helpText("3. ", strong("Governance (G)"), ": This focuses on a company’s leadership, transparency, and ethics."),
      
      selectInput("company1", "Select First Company:", choices = unique(data$name)),
      selectInput("company2", "Select Second Company:", choices = unique(data$name)),
      
      helpText("How to use these charts:"),
      helpText("1. Select two companies from the dropdown menus to compare their ESG performance."),
      helpText("2. Read each axis label to understand which ESG score is being displayed (Environmental, Social, Governance)."),
      helpText("3. Each axis shows the company's performance in that area. Higher scores indicating better performance.")
    ),
    
    mainPanel(
      plotlyOutput("esgRadar1"),
      plotlyOutput("esgRadar2")
    )
  )
)

server <- function(input, output) {
  
  output$esgRadar1 <- renderPlotly({
    company1_scores <- data %>%
      filter(name == input$company1) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    radar_data1 <- data.frame(
      metric = c("Environmental Score", "Social Score", "Governance Score"),
      score = company1_scores
    )
    
    plot_ly(
      type = 'scatterpolar',
      r = radar_data1$score,
      theta = radar_data1$metric,
      fill = 'toself',
      text = paste0(radar_data1$metric, ": ", radar_data1$score),
      hoverinfo = 'text'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(data[c("environment_score", "social_score", "governance_score")])),
            title = "Score"
          )
        ),
        title = paste("ESG Performance for", input$company1)
      )
  })
  
  output$esgRadar2 <- renderPlotly({
    company2_scores <- data %>%
      filter(name == input$company2) %>%
      select(environment_score, social_score, governance_score) %>%
      unlist() %>%
      as.numeric()
    
    radar_data2 <- data.frame(
      metric = c("Environmental Score", "Social Score", "Governance Score"),
      score = company2_scores
    )
    
    plot_ly(
      type = 'scatterpolar',
      r = radar_data2$score,
      theta = radar_data2$metric,
      fill = 'toself',
      text = paste0(radar_data2$metric, ": ", radar_data2$score),
      hoverinfo = 'text'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(data[c("environment_score", "social_score", "governance_score")])),
            title = "Score"
          )
        ),
        title = paste("ESG Performance for", input$company2)
      )
  })
}

shinyApp(ui = ui, server = server)


