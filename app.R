# Install and load shiny package if you don't have it already
# install.packages("shiny")
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny App with Plots and Filters"),
  
  # Sidebar layout with input filters and plot output
  sidebarLayout(
    sidebarPanel(
      
      # Select input for number of cylinders
      selectInput("cylinders", 
                  label = "Select Number of Cylinders:",
                  choices = unique(mtcars$cyl), 
                  selected = unique(mtcars$cyl),
                  multiple = TRUE),
      
      # Select input for number of gears
      selectInput("gears", 
                  label = "Select Number of Gears:",
                  choices = unique(mtcars$gear), 
                  selected = unique(mtcars$gear),
                  multiple = TRUE)
    ),
    
    # Main panel for displaying plots
    mainPanel(
      plotOutput("scatterPlot"),
      plotOutput("barPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive data subset based on input filters
  filteredData <- reactive({
    mtcars[mtcars$cyl %in% input$cylinders & mtcars$gear %in% input$gears, ]
  })
  
  # Scatter plot of mpg vs wt with filtered data
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes(x = wt, y = mpg, color = factor(cyl))) +
      geom_point(size = 3) +
      labs(title = "Scatter Plot of MPG vs Weight", x = "Weight (wt)", y = "Miles per Gallon (mpg)") +
      theme_minimal()
  })
  
  # Bar plot of the number of cars for each combination of cylinders and gears
  output$barPlot <- renderPlot({
    ggplot(filteredData(), aes(x = factor(cyl), fill = factor(gear))) +
      geom_bar(position = "dodge") +
      labs(title = "Bar Plot of Cylinders and Gears", x = "Number of Cylinders", y = "Count") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
