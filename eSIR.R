source("israel_functions.R")
library(deSolve)
library(outbreaks)
library(gridExtra)
library(arm)
library(tidyverse)
library(bbmle)
library(zoo)
library(eSIR)
library(lubridate)
library(dplyr)
library(ggplot2)

x <- "11/14/2006 20:10"





date0 <- as.Date("2020-12-27")



date1 <- as.Date("2021-01-07")
date2 <- as.Date("2021-02-07")
date3 <- as.Date("2021-03-07")
date4 <- as.Date("2021-04-18")


date_initial = date0
date_final = date4

israel <- israel_data_only(date_initial, date_final)

israel = israel %>%
  mutate(period = ifelse(date < date1, 1,
                         ifelse(date >= date1 & date < date2, 2,
                                ifelse(date >= date2 & date < date3, 3,
                                       ifelse(date >= date3, 4, 0)))))

NI_complete = israel$cases_total

RI_complete = israel$total_removed

N=9449000

R <- RI_complete / N
Y <- NI_complete / N - R


change_time <- c("01/07/2021", "02/07/2021", "03/07/2021")
pi0 = get_R_data()$R %>% mutate(death_07da = zoo::rollmeanr(`Mean(R)`, k = 7, fill = NA))

R = R[1:nrow(pi0)]
Y = Y[1:nrow(pi0)]

pi0$date = as.Date(date0)
for(i in 1:nrow(pi0)){
  pi0$date[i] = israel$date[i]
  if(is.na(pi0$death_07da[i])){
    
      pi0$death_07da[i] = pi0$death_07da[7]
  }
}


pi0 = pi0 %>% rename(Mean_R  = `Mean(R)`)

pi0 = pi0 %>% mutate(pi = Mean_R/death_07da)

pi0 = pi0 %>% mutate(date = format(date, format = "%m/%d/%Y"))

pi0$Y = Y
pi0$R = R

change_time = pi0$date

change_time = change_time[2:length(change_time)-1]



model = tvt.eSIR(Y = pi0$Y, R = pi0$R, pi0 = pi0$pi, T_fin = 150, begin_str = pi0$date[1], save_plot_data = TRUE, change_time = change_time)

I_proj = colMeans(model$theta_pp[, , 2], na.rm = TRUE)
R_proj = colMeans(model$theta_pp[, , 3], na.rm = T) 

as.Date(seq(as.Date("2021/04/10"), by = "day", length.out = 5), format("%B-%d-%y"))
a = seq(as.Date("2021/04/10"), by = "day", length.out = 45)
b = c()
for(i in 1:length(a)){
  b = c(b, format(a[i], format = "%m/%d/%y"))
}

b = as.Date(b, format = "%m/%d/%y")

eSIR_df = data.frame(date = b, preds = model$Y_mean, ym = model$Y_band$lower, ymax = model$Y_band$upper)

ggplot(data = eSIR_df, aes(x = date, y = preds)) + scale_x_date(date_breaks = "1 week", labels = scales::date_format("%e %b"))+ geom_line() + geom_ribbon(mapping = aes(x = date, ymin = ym, ymax = ymax), alpha = 0.2)
  
get_R_data = function(){
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
  
  # Static plots
  ggplot(israel) +
    aes(x = date, y = I) +
    geom_col()
  
  israel =
    israel %>%
    filter(date > as.Date("2020-12-27"),
           date < as.Date("2021-04-18"))
  
  # set the mean serial interval
  serial_interval = 4.0
  serial_sd = (4.9 - 4.0) / 1.96
  # 95% interval = 3.1 to 4.9
  
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
  

  
  return (res)
}

