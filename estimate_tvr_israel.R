# Read raw COVID-19 data from JHU
# Gist: https://gist.github.com/mkleinsa/9b4fb698d348d5d4d2e475253334bb2b

# Github folder:
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

# load packages
# Install if needed: install.packages("tidyverse")
library(tidyverse)
library(plotly)


tvr_plot = function(){



# confirmed
case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed = read_csv(case_url)

# deaths
death_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths = read_csv(death_url)

# recoveries
recovery_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recoveries = read_csv(recovery_url)

# Next steps: Inspect the data with
# Process the data with dplyr (or software of your choice)
# Data format should be
#      - each row is a day (with date)
#      - the columns are daily cases, deaths, recoveries.

# -----

# Multiple regions under this country?
confirmed %>% filter(`Country/Region` == "Israel") %>%
  pull(`Province/State`) %>% unique()

israel_confirmed = confirmed %>%
  filter(`Country/Region` == "Israel") %>% #, is.na(`Province/State`)
  dplyr::select(-`Province/State`, -Lat, -Long, -`Country/Region`)
israel_confirmed = tibble(date = colnames(israel_confirmed),
                          cases_total = as.numeric(israel_confirmed[1, ]))

# process/merge deaths and recoveries the in the same fashion

# -----

israel_deaths = deaths %>%
  filter(`Country/Region` == "Israel") %>% #, is.na(`Province/State`)
  dplyr::select(-`Province/State`, -Lat, -Long, -`Country/Region`)
israel_deaths = tibble(date = colnames(israel_deaths),
                       deaths_total = as.numeric(israel_deaths[1, ]))

israel_recovered = recoveries %>%
  filter(`Country/Region` == "Israel") %>% #, is.na(`Province/State`)
  dplyr::select(-`Province/State`, -Lat, -Long, -`Country/Region`)
israel_recovered = tibble(date = colnames(israel_recovered),
                          recoveries_total = as.numeric(israel_recovered[1, ]))

israel = israel_confirmed %>% left_join(israel_recovered, by = "date")
israel = israel %>% left_join(israel_deaths, by = "date")

# Transform data for plotting
as.Date("1/24/20", format = "%m/%d/%y")
israel =
  israel %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

# daily case, death, recoveries
israel =
  israel %>%
  mutate(total_removed = deaths_total + recoveries_total,
         total_active_infected = cases_total - total_removed,
         day_index = 1:n(),
         daily_removed = total_removed - lag(total_removed),
         daily_infected = cases_total - lag(cases_total),
         I = daily_infected,
         R = daily_removed) %>% filter (R > -3e4) %>% filter (I >= 0)

vline <- function(x, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}


# Static plots
ggplot(israel) +
  aes(x = date, y = I) +
  geom_col()

israel =
  israel %>%
  filter(date > as.Date("2020-12-27"),
         date < as.Date("2021-04-18"))

# set the mean serial interval
serial_interval = 3.18
serial_sd = (3.81 - 3.18) / 1.96
# 95% interval = 2.55 to 3.81

# for a five day window
t_start = seq(2, nrow(israel) - 4)
t_end = t_start + 4

res <- EpiEstim::estimate_R(
  incid = israel$daily_infected,
  method = "parametric_si",
  config = EpiEstim::make_config(list(
    mean_si             = serial_interval,
    std_si              = serial_sd,
    si_parametric_distr = "G",
    t_start             = t_start,
    t_end               = t_end,
    seed                = 46342))
)

plot(res)

start_date = israel$date[1]

# fancy plot
plt_data <- tibble(
  date_num = res$dates
) %>% left_join(
  res$R, by = c("date_num" = "t_end")
) %>%
  dplyr::select(
    date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
  ) %>%
  add_column(date = israel$date) %>%
  mutate(
    text = paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                  format(round(r, 2), nsmall = 2), "<br>CI: ",
                  paste0("[", format(round(lower, 2), nsmall = 2), ", ",
                         format(round(upper, 2), nsmall = 2), "]"))
  ) %>%
  filter(!is.na(r))

titlesize <- list(size = 20)
tickfont <- list(size=18)

p <- plot_ly(plt_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
             line = list(color = "rgb(9, 131, 179)", width = 5),
             hoverinfo = "text",
             text   = ~text) %>%
  add_markers(data = plt_data, x = ~date, y = ~r, mode = "marker",
              marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%
  add_ribbons(ymin = ~lower,
              ymax = ~upper,
              line = list(color = 'rgba(2, 180, 250, 0.05)'),
              fillcolor = 'rgba(2, 180, 250, 0.2)',
              hoverinfo = "none") %>%
  
  
  add_segments(x = as.Date("2021-01-07"), xend = as.Date("2021-01-07"), y = .5, yend = 2, line = list(color = 'black', width = 2, dash='dot')) %>%
  add_segments(x = as.Date("2021-02-07"), xend = as.Date("2021-02-07"), y = .5, yend = 2, line = list(color = 'black', width = 2, dash='dot')) %>%
  add_segments(x = as.Date("2021-03-07"), xend = as.Date("2021-03-07"), y = .5, yend = 2, line = list(color = 'black', width = 2, dash='dot')) %>%
  
  
  layout(
    title = list(text = "Time-Varying Reproduction Number (TVR) Plot", xanchor = "left", x = 0,
                 titlefont = titlesize),
    xaxis = list(title = "Date (2021)", 
                 titlefont = titlesize,
                 tickfont = tickfont,
                 zeroline = T),
    yaxis = list(title = "R(t)",
                 titlefont = titlesize,
                 tickfont = tickfont,
                 zeroline = T),
    shapes = list(
      type = "line", xref = "paper", yref = "data",
      x0 = 0, x1 = 1, y0 = 1, y1 = 1,
      line = list(color = "rgba(201, 40, 40, 0.5)")
    ),
    showlegend = FALSE
  ) %>%
  plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

return(p)
}
tvr_plot()
