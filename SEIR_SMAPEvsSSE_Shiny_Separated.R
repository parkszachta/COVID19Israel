library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("Israel Covid Dashboard"),

  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Infected", plotOutput("infected_plot")),
                tabPanel("Removed", plotOutput("removed_plot")),
                tabPanel("TPR", plotlyOutput("tpr_plot")),
                tabPanel("TVR", plotlyOutput("tvr_plot"))
    )
  )
)





server <- function(input, output) {

load("output/model_results.RData")



  output$infected_plot <- renderPlot({
    results[[1]]
  })
  output$removed_plot <- renderPlot({
    results[[2]]
  })
  output$tpr_plot <- renderPlotly({
    results[[3]]
  })
  output$tvr_plot <- renderPlotly({
    results[[4]]
  })



}



# Run the application
shinyApp(ui = ui, server = server)
