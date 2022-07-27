library(readr)
library(tidyverse)
library(plotly)

positive_rate <- function() {
  link = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
  df = read_csv(link)
  View(df)
  df = df %>%
       filter(`location` == "Israel") %>%
       dplyr::select(date, positive_rate, new_tests, new_cases, new_cases_smoothed, new_tests_smoothed, people_fully_vaccinated_per_hundred, new_deaths_smoothed) %>%
       mutate(calc_positive_rate = new_cases / new_tests, smooth_positive_rate = new_cases_smoothed / new_tests_smoothed, prop_fully_vaccinated = people_fully_vaccinated_per_hundred / 100, new_deaths_adj = new_deaths_smoothed / 200)
  orange = "#d55e00"
  blue = "#0072b2"
  green = "#009e73"
  p_calculated = plot_ly(data = df, x = ~date, y = ~calc_positive_rate, type = "scatter",
               mode = "lines", line = list(color = orange), name = "Calculated TPR") %>%
               add_trace(data = df, x = ~date, y = ~positive_rate, type = "scatter",
                         mode = "lines", line = list(color = blue), name = "Reported TPR") %>%
               add_trace(data = df, x = ~date, y = ~prop_fully_vaccinated, type = "scatter",
                         mode = "lines", line = list(color = green), name = "Proportion Fully Vaccinated") %>%
               layout(title = "Test Positivity Rate in Israel (TPR) with Proportion Fully Vaccinated",
                      xaxis = list(title = "Date"),
                      yaxis = list(title = "TPR"))
  p_calculated
}

positive_rate()
