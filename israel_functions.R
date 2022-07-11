israel_data_only <- function(date_initial, date_final){


### Import the data

case_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed = read_csv(case_url)

# deaths
death_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths = read_csv(death_url)

# recoveries
recovery_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recoveries = read_csv(recovery_url)

# Process and merge data ----
israel_confirmed = confirmed %>%
  filter(`Country/Region` == "Israel") %>% #, is.na(`Province/State`)
  dplyr::select(-`Province/State`, -Lat, -Long, -`Country/Region`)
israel_confirmed = tibble(date = colnames(israel_confirmed),
                          cases_total = as.numeric(israel_confirmed[1, ]))

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

israel =
  israel %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

israel =
  israel %>%
  mutate(total_removed = deaths_total + recoveries_total,
         active_cases = cases_total - total_removed,
         day = 1:n(),
         I = active_cases,
         R = total_removed)

israel =
  israel %>%
  filter(I < 2e7, R > -3e5)

israel =
  israel %>%
  filter(date >= date_initial, date <= date_final)

return (israel)
}

# SIR solver ----
sir_1 = function(beta, gamma, I0, R0, times, N, lambda, mu) {
  # define SIR equations
  sir_equations = function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS = -beta * I * S/N + lambda * N - mu * S
      dI =  beta * I * S/N - gamma * I -mu * I
      dR =  gamma * I - mu*R
      return(list(c(dS, dI, dR)))
    })
  }
  # prepare input for ODE solver
  parameters_values = c(beta = beta, gamma = gamma)
  S0 = N - I0 - R0
  initial_values = c(S = S0, I = I0, R = R0)
  # solve system of ODEs
  out = ode(initial_values, times, sir_equations, parameters_values,method = "rk4")
  return(as.data.frame(out))
}


seir_1 = function(beta, gamma, sigma, E0, I0, R0, times, N, lambda, mu) {
  # define SEIR equations
  sir_equations = function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS = -beta * I * S/N + lambda * N - mu * S 
      dE = beta * I * S/N - sigma * E - mu * E
      dI =  sigma*E - gamma * I -mu * I
      dR =  gamma * I - mu * R
      return(list(c(dS, dE, dI, dR)))
    })
  }
  # prepare input for ODE solver
  parameters_values = c(beta = beta, gamma = gamma, sigma = sigma)
  S0 = N - I0 - R0 - E0
  initial_values = c(S = S0, I = I0, R = R0, E = E0)
  # solve system of ODEs
  out = ode(initial_values, times, sir_equations, parameters_values,method = "rk4")
  return(as.data.frame(out))
}


# Calculate sums of squares ----
ss_SIR = function(beta, gamma, N, data, lambda, mu) {
  # starting cases and removals on day 1
  I0 = data$I[1]
  R0 = data$R[1]
  times = data$day
  # transform parameters so they are non-negative
  beta = exp(beta)
  gamma = exp(gamma)
  # generate predictions using parameters, starting values
  predictions = sir_1(beta = beta, gamma = gamma,                        # parameters
                      I0 = I0, R0 = R0,                                  # variables' intial values
                      times = times, N = N, lambda = lambda, mu = mu)    # time points
  # compute the sums of squares
  sum((predictions$I[-1] - data$I[-1])^2 + (predictions$R[-1] - data$R[-1])^2)
  #sum((predictions$I[-1] - data$I[-1])^2 )
}


# convenient wrapper to return sums of squares ----
ss2_SIR = function(x, N, data, lambda, mu) {
  ss_SIR(beta = x[1], gamma = x[2], N = N, data = data, lambda = lambda, mu = mu)
}

ss_SEIR = function(beta, gamma, sigma, N, data, lambda, mu) {
  # starting cases and removals on day 1
  I0 = data$I[1]
  R0 = data$R[1]
  # E0 = (data$cases_total[2] - data$cases_total[1]) / sigma
  E0 = I0 * 3
  times = data$day
  # transform parameters so they are non-negative
  beta = exp(beta)
  gamma = exp(gamma)
  # generate predictions using parameters, starting values
  predictions = seir_1(beta = beta, gamma = gamma, sigma = sigma,                       # parameters
                      I0 = I0, R0 = R0, E0 = E0,                              # variables' intial values
                      times = times, N = N, lambda = lambda, mu = mu)    # time points
  # compute the sums of squares
  sum((predictions$I[-1] - data$I[-1])^2 + (predictions$R[-1] - data$R[-1])^2)
  #sum((predictions$I[-1] - data$I[-1])^2 )
}

# convenient wrapper to return sums of squares 
ss2_SEIR = function(x, N, data, lambda, mu, sigma) {
  ss_SEIR(beta = x[1], gamma = x[2], N = N, data = data, lambda = lambda, mu = mu, sigma = sigma)
}

#loglikelihood function for mle
logli_SIR = function(beta, gamma, N, dat, lambda, mu) {
  I0 = dat$I[1]
  R0 = dat$R[1]
  times = dat$day
  beta = exp(beta)
  gamma = exp(gamma)
  predictions = sir_1(beta = beta, gamma = gamma,   # parameters
                      I0 = I0, R0 = R0, # variables' intial values
                      times = times, N = N, lambda = lambda, mu = mu)
  ## negative of log likelihood
  -sum(dpois(x = dat$I, lambda = predictions$I, log = TRUE)) - sum(dpois(x = dat$R, lambda = predictions$R, log = TRUE))
}

logli_SEIR = function(beta, gamma, sigma, N, dat, lambda, mu) {
  I0 = dat$I[1]
  R0 = dat$R[1]
  # E0 = (dat$cases_total[2] - dat$cases_total[1]) / sigma
  E0 = I0 * 3
  times = dat$day
  beta = exp(beta)
  gamma = exp(gamma)
  predictions = seir_1(beta = beta, gamma = gamma, sigma = sigma,  # parameters
                      I0 = I0, R0 = R0, E0 = E0, # variables' intial values
                      times = times, N = N, lambda = lambda, mu = mu)
  ## negative of log likelihood
  -sum(dpois(x = dat$I, lambda = predictions$I, log = TRUE)) - sum(dpois(x = dat$R, lambda = predictions$R, log = TRUE))
}
