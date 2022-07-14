library(shiny)
source("Israel_SEIR_predictions.R")
source("positive_rate.R")

israel <- as.data.frame(israel_pred_df())

date0 <- as.Date("2020-12-27")
date1 <- as.Date("2021-01-07")
date2 <- as.Date("2021-02-07")
date3 <- as.Date("2021-03-07")
date4 <- as.Date("2021-04-18")

date_initial = date0
date_final = date4



israel$pred_I = unlist(israel$pred_I)
israel$date = unlist(israel$date)
israel$pred_R = unlist(israel$pred_R)
israel$I = unlist(israel$I)
israel$R = unlist(israel$R)



ui <- fluidPage(
  
  # Application title
  titlePanel("SEIR Active Cases/Recovered"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Infected", plotOutput("infected_plot")), 
                tabPanel("Removed", plotOutput("recovered_plot")),
                tabPanel("TPR", plotlyOutput("tpr_plot"))
    )
  )
)





server <- function(input, output) {
  
  p1 <- SEIR_plot1(israel)
  p2 <- SEIR_plot2(israel)
  tpr <- positive_rate()
  
  
  output$infected_plot <- renderPlot({
    p1
  })
  output$recovered_plot <- renderPlot({
    p2
  })
  output$tpr_plot <- renderPlotly({
    tpr
  })
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)



