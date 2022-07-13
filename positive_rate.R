library(readr)
library(tidyverse)
library(plotly)

positive_rate <- function() {
  link = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  df = read_csv(link)
  df = df %>%
       filter(`location` == "Israel") %>%
       select(date, positive_rate, new_tests, new_cases, new_cases_smoothed, new_tests_smoothed) %>%
       mutate(calc_positive_rate = new_cases / new_tests, smooth_positive_rate = new_cases_smoothed / new_tests_smoothed)
  red = "#FF0000"
  green = "#00FF00"
  blue = "#0000FF"
  p_calculated = plot_ly(data = df, x = ~date, y = ~calc_positive_rate, type = "scatter",
               mode = "lines", line=list(color=red), name="Calculated TPR") %>%
               add_trace(data = df, x = ~date, y = ~positive_rate, type = "scatter",
                         mode = "lines", line=list(color=green), name="Reported TPR") %>%
               add_trace(data = df, x = ~date, y = ~smooth_positive_rate, type = "scatter",
                         mode = "lines", line=list(color=blue), name="Calculated Smooth TPR") %>%
               layout(title = "Test Positivity Rate in Israel (TPR)",
                      xaxis = list(title = "Date"),
                      yaxis = list(title = "TPR"))
  p_calculated
}

positive_rate()
