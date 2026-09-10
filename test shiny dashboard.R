library(shiny)
library(semantic.dashboard)
library(DT)

ui <- dashboardPage( theme = "superhero",
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Iris", tabName = "iris", icon = icon("tree")),
      menuItem("Cars", tabName = "cars", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "iris",
              box(plotOutput("correlation_plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("Sepal.Width", "Petal.Length", "Petal.Width")),
                width = 4
              )
      ),
      tabItem(tabName = "cars",
              fluidPage(
                h1("Cars"),
                dataTableOutput("carstable")
              )
      )
    )
  )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]], xlab= "Sepal Length", ylab = "Feature")
  })
  output$carstable <- renderDataTable(mtcars)
}

shinyApp(ui, server)

