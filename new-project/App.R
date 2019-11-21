library(shiny)
library(ProjectTemplate)
library(ggplot2)
source("./munge/functions.R")
load.project()

# Define UI ----
ui <- fluidPage(
  # ----
  # Title
  titlePanel("Data Management and Explore Data Analysis------Online Learning"),
  # ----
  # Business Understanding Text
  
  # ----
  sidebarLayout(
    sidebarPanel(
      h5("Data Statistics & Understanding about correct rate"),
      selectInput("fid", "The Term choice: ", choices=c("all", 1,2,3,4,5,6,7), selected = 3),
      hr(),
      selectInput("feature", "The Feature choice: ", choices=c("gender", "country", "age_range", "highest_education_level", "employment_status", "employment_area", "detected_country")),
      hr(),
      ),
    mainPanel(
      plotOutput("correct_rate_plot")
    )
  )
)

# Define server logic ----
  server <- function(input, output) {
  output$correct_rate_plot <- renderPlot({
    # generate an rnorm distribution and plot it
    tmp  = main(input$fid, input$feature)
    ggplot(tmp, mapping = aes_string(x="correct_rate", y="ratio", fill=as.character(input$feature))) + geom_bar(stat = "identity",position = "dodge", width = 0.2)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)