library(shiny)
library(plotly)
library(dplyr)
library(DT)

data <- read.csv("Electric_Vehicle_Population_Data.csv") %>%
  filter(complete.cases(.)) %>%    
  filter(Base.MSRP > 0, Electric.Range > 0) %>% 
  distinct()     

data$Model.Year <- as.integer(data$Model.Year)

ui <- fluidPage(
  titlePanel("Electric Vehicle Finder"),
  
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
                  sep = "")
    ),
    mainPanel(
      plotlyOutput("scatterPlot", width = "100%", height = "650px"),
      DTOutput("carTable")
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
        if (!is.null(input$makeInput)) Make %in% input$makeInput else TRUE
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
        "Year:", Model.Year
      ),
      hoverinfo = "text",
      type = "scatter3d",
      mode = "markers"
    ) %>%
      layout(
        title = "Electric Vehicle Finder",
        scene = list(
          xaxis = list(title = "Vehicle Range (miles)"),
          yaxis = list(title = "Base MSRP ($)"),
          zaxis = list(title = "Year", tickformat = "d")
        )
      )
  })
  
  selectedPoints <- reactive({
    event_data("plotly_selected") %>% 
      {if (is.null(.)) filteredData() else .} %>% 
      as.data.frame()
  })
  
  output$carTable <- renderDT({
    selectedData <- selectedPoints()
    if (nrow(selectedData) > 0) {
      filteredData() %>%
        filter(
          Electric.Range %in% selectedData$Electric.Range &
            Base.MSRP %in% selectedData$Base.MSRP
        ) %>%
        select(Make, Model, Electric.Range, Base.MSRP, Model.Year) %>%
        arrange(Make, Model)
    } else {
      filteredData() %>%
        select(Make, Model, Electric.Range, Base.MSRP, Model.Year) %>%
        arrange(Make, Model)
    }
  })
}

shinyApp(ui = ui, server = server)



