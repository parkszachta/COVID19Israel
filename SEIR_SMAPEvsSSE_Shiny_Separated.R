library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("Israel COVID-19 Dashboard"),

  # Show a plot of the generated distribution
  mainPanel(
    h4("This uses the Susceptible, Exposed, Infectious, and Removed (SEIR) framework
        to model the number of Infectious individuals over time."),
    h4("This uses the Susceptible, Exposed, Infectious, and Removed (SEIR) framework
        to model the number of Removed individuals over time. This includes people
        who have recovered from COVID-19, in addition to those who died from it."),
    h4("This is a plot of the Test Positivity Rate (TPR) in Israel, which is the
       proportion of COVID-19 tests that were positive. The red plot is from
       simply dividing the numbers, and the blue plot is the reported data, implying
       a smoothing function was used. The proportion of people who are fully vaccinated
       is provided in green for reference."),
    h4("This is a model of the Time-Varying R (TVR) graph, which represents the
       expected number of secondary infections that will result from
       an Infectious person. An R(t) value is less than 1, it is likely a sign of
       decreasing cases, and an R(t) value of greater than 1 is a likely a sign of
       increasing cases."),
    tabsetPanel(type = "tabs",
                tabPanel("SIR Active Cases", plotlyOutput("SIRinfected_plot")),
                tabPanel("SIR Total Removed", plotlyOutput("SIRremoved_plot")),
                tabPanel("SEIR Active Cases", plotlyOutput("SEIRinfected_plot")),
                tabPanel("SEIR Total Removed", plotlyOutput("SEIRremoved_plot")),
                tabPanel("eSIR Active Cases", plotlyOutput("eSIRinfected_plot")),
                tabPanel("eSIR Total Removed", plotlyOutput("eSIRremoved_plot")),
                tabPanel("TPR", plotlyOutput("tpr_plot")),
                tabPanel("TVR", plotlyOutput("tvr_plot"))             
    ),
    p("Cases Data: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"),
    p("Deaths Data: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"),
    p("Recovery Data: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"),
    p("Serial Interval Length: https://www.researchgate.net/publication/354483479_Serial_interval_of_COVID-19_and_the_effect_of_Variant_B117_analyses_from_prospective_community_cohort_study_Virus_Watch"),
    p("Policy Information: https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Israel")

  )
)





server <- function(input, output) {

load("output/model_results.RData")



  output$SEIRinfected_plot <- renderPlotly({
    results[[1]]
  })
  output$SEIRremoved_plot <- renderPlotly({
    results[[2]]
  })
  output$tpr_plot <- renderPlotly({
    results[[3]]
  })
  output$tvr_plot <- renderPlotly({
    results[[4]]
  })
  output$SIRinfected_plot <- renderPlotly({
    results[[5]]
  })
  output$SIRremoved_plot <- renderPlotly({
    results[[6]]
  })
  output$eSIRinfected_plot <- renderPlotly({
    results[[7]]
  })
  output$eSIRremoved_plot <- renderPlotly({
    results[[8]]
  })


}



# Run the application
shinyApp(ui = ui, server = server)
