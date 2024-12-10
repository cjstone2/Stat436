library(shiny)
library(plotly)
library(dplyr)
library(DT)

data <- read.csv("https://www.dropbox.com/scl/fi/aarlsjjysy50zlkb49n6p/Electric_Vehicle_Population_Data.csv?rlkey=7osmeq4r0kualodwafprsdpma&st=0n25517o&dl=1") %>%
  filter(complete.cases(.)) %>%    
  filter(Base.MSRP > 0, Electric.Range > 0) %>% 
  distinct()     

data$Model.Year <- as.integer(data$Model.Year)

ui <- fluidPage(
  titlePanel("Electric and Hybrid Vehicle Finder"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("rangeInput", "Vehicle Range (miles):",
                  min = min(data$Electric.Range), max = max(data$Electric.Range),
                  value = c(min(data$Electric.Range), max(data$Electric.Range)),
                  step = 25),
      sliderInput("priceInput", "Base MSRP ($):",
                  min = min(data$Base.MSRP), max = max(data$Base.MSRP),
                  value = c(min(data$Base.MSRP), max(data$Base.MSRP)),
                  step = 100000),
      selectInput("makeInput", "Brand:",
                  choices = unique(data$Make),
                  selected = "TESLA",
                  multiple = TRUE),
      sliderInput("yearInput", "Model Year:",
                  min = min(data$Model.Year), max = max(data$Model.Year),
                  value = c(min(data$Model.Year), max(data$Model.Year)),
                  step = 1,
                  sep = ""),
      selectInput("evTypeInput", "Electric Vehicle Type:",
                  choices = unique(data$Electric.Vehicle.Type),
                  selected = "Battery Electric Vehicle (BEV)",
                  multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput("scatterPlot", width = "100%", height = "650px")
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    data %>%
      filter(
        Electric.Range >= input$rangeInput[1],
        Electric.Range <= input$rangeInput[2],
        Base.MSRP >= input$priceInput[1],
        Base.MSRP <= input$priceInput[2],
        Model.Year >= input$yearInput[1],
        Model.Year <= input$yearInput[2],
        if (!is.null(input$makeInput)) Make %in% input$makeInput else TRUE,
        Electric.Vehicle.Type == input$evTypeInput
      )
  })
  
  output$scatterPlot <- renderPlotly({
    plot_ly(
      data = filteredData(),
      x = ~Electric.Range,
      y = ~Base.MSRP,
      z = ~Model.Year,
      color = ~Make,
      text = ~paste(
        "Make:", Make, "<br>",
        "Model:", Model, "<br>",
        "Range:", Electric.Range, "miles<br>",
        "Price:", Base.MSRP, "<br>",
        "Year:", Model.Year, "<br>",
        "Type:", Electric.Vehicle.Type
      ),
      hoverinfo = "text",
      type = "scatter3d",
      mode = "markers"
    ) %>%
      layout(
        title = "Electric/Hybrid Vehicle Finder",
        scene = list(
          xaxis = list(title = "Vehicle Range (miles)"),
          yaxis = list(title = "Base MSRP ($)"),
          zaxis = list(title = "Year", tickformat = "d")
        )
      )
  })
}

shinyApp(ui = ui, server = server)



