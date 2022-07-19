library(shiny)
options(scipen=999)
source("positive_rate.R")
source("estimate_tvr_israel.R")
source("SEIR_SMAPEvsSSE.R")


israel <- as.data.frame(israel_pred_df())
israel <- israel[-11]


date0 <- as.Date("2020-12-27")
date1 <- as.Date("2021-01-07")
date2 <- as.Date("2021-02-07")
date3 <- as.Date("2021-03-07")
date4 <- as.Date("2021-04-18")

date_initial = date0
date_final = date4


israel <-  israel %>% rename(pred_I_SMAPE = 11,
                             pred_R_SMAPE = 12,
                             pred_I_SSE = 13,
                             pred_R_SSE = 14)

israel$pred_I_SMAPE = unlist(israel$pred_I_SMAPE)
israel$pred_I_SMAPE = unlist(israel$pred_I_SMAPE)
israel$pred_R_SSE = unlist(israel$pred_R_SSE)
israel$pred_R_SSE = unlist(israel$pred_R_SSE)
israel$date = unlist(israel$date)
israel$I = unlist(israel$I)
israel$R = unlist(israel$R)


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
  
  p1 <- SEIR_plot1(israel)
  p2 <- SEIR_plot2(israel)
  tpr <- positive_rate()
  tvr <- tvr_plot()
  
  
  
  output$infected_plot <- renderPlot({
    p1
  })
  output$removed_plot <- renderPlot({
    p2
  })
  output$tpr_plot <- renderPlotly({
    tpr
  })
  output$tvr_plot <- renderPlotly({
    tvr
  })
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)