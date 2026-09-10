library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage( skin = "red",
                     dashboardHeader(title = "Testdashboard"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Iris", tabName = "iris", icon = icon("tree")),
                         menuItem("Auto's", tabName = "cars", icon = icon("car"))
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "iris",
                                 box(plotOutput("correlation_plot"), width = 8),
                                 box(
                                   selectInput("features", "Kenmerken:",
                                               c("Kelkbladbreedte" = "Sepal.Width",
                                                 "Kroonbladlengte" = "Petal.Length",
                                                 "Kroonbladbreedte" = "Petal.Width")),
                                   width = 4
                                 )
                         ),
                         tabItem(tabName = "cars",
                                 fluidPage(
                                   h1("Auto's"),
                                   dataTableOutput("carstable")
                                 )
                         )
                       )
                     )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]], xlab= "Kelkbladlengte", ylab = "Kenmerk")
  })
  output$carstable <- renderDataTable(mtcars)
}

shinyApp(ui, server)