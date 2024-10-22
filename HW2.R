library(shiny)
library(tidyverse)

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv') |>
  mutate(date = as.Date(date), year = lubridate::year(date))

ui <- fluidPage(
  titlePanel("Mario Kart World Records"),
  
  p("This Shiny app allows you to explore Mario Kart record times for different tracks. 
     This data is from the TidyTuesday GitHub and sourced from https://mkwrs.com/"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("track", "Select Track:", 
                  choices = unique(records$track)),
      
      sliderInput("year", "Year", 1997, 2021, c(1997, 2021), sep = ""),
      
      helpText("Use the selection box to choose a track and the slider to filter records by year. 
                You can brush over the scatter plot to highlight information in the table.")
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "625px", brush = "plot_brush"), 
      tableOutput("recordTable")
    )
  )
)

server <- function(input, output, session) {
  filtered_records <- reactive({
    req(input$track)  
    records %>%
      filter(track == input$track, year >= input$year[1], year <= input$year[2])
  })
  
  selected_points <- reactiveVal(rep(TRUE, nrow(records)))
  
  observeEvent(input$plot_brush, {
    selected_points(brushedPoints(filtered_records(), input$plot_brush, 
                                  allRows = TRUE)$selected_)
  })
  
  output$scatterPlot <- renderPlot({
    data <- filtered_records()  
    ggplot(data, aes(x = time, y = player, color = time)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Record Times by Player", 
           x = "Time (seconds)", 
           y = "Player",
           color = "Time (seconds)") +
      theme(
        axis.text.y = element_text(angle = 0, hjust = 1, size = 15)  
      )
  })
  
  output$recordTable <- renderTable({
    data <- filtered_records()  
    data %>%
      filter(selected_points()) |>
      mutate(date = format(date, "%Y-%m-%d")) |>       
      select(track, type, shortcut, player, system_played, date, time, year) |>
      arrange(time)
  })
}

shinyApp(ui = ui, server = server)
