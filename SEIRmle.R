# Title:

# Authors:

# Packages ----
library(deSolve)
library(outbreaks)
library(gridExtra)
library(arm)
library(tidyverse)
library(bbmle)

source("israel_functions.R")

israel_info <- function(){

# Set date ranges ----
date_initial = as.Date("2020-12-27")
date_final = as.Date("2021-04-18")

# Import data ----
# confirmed
israel <- israel_data_only(date_initial, date_final)

starting_param_val = log(c(1e-2,1e-5))
N = 1366e6                                 # population size
lambda = mu = 0                            # birth/death rate
data = israel                               # set the data set

beta = 2
gamma = 0.05
I0 = 5
R0 = 2

times = data$day - 70

predictions = sir_1(beta = beta, gamma = gamma,                        # parameters
                    I0 = I0, R0 = R0,                                  # variables' intial values
                    times = times, N = N, lambda = lambda, mu = mu)

predictions =
  predictions %>%
  pivot_longer(cols = c(S, I, R))

method = "mle"

if(method == "ls"){
  # set starting values ----
  starting_param_val = log(c(1e-2,1e-5))
  N = 1366e6                                 # population size
  lambda = mu = 0                            # birth/death rate
  data = israel                               # set the data set
  
  # Optimization result ----
  ss_optim = optim(starting_param_val, ss2, N = N, data = data, lambda = lambda,
                   mu = mu)
  
  # Obtain beta, gamma
  pars = ss_optim$par
}
if(method == "mle"){
  N=1366e6
  lambda=mu=0
  
  starting_param_val = list(beta = 1e-2, gamma = 1e-5)
  estimates_pois = mle2(minuslogl = logli,
                        start = lapply(starting_param_val, log), method = "Nelder-Mead",
                        data=list(dat = data, N = N, lambda = lambda, mu = mu))
  pars = as.numeric(coef(estimates_pois))
}


R = as.numeric(exp(pars[1]) / exp(pars[2]))           # compute R0 for SIR

# Predictions ----
predictions = sir_1(beta = exp(pars[1]), gamma = exp(pars[2]), I0 = data$I[1],
                    R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                    mu = mu)              # generate predictions from the least
# squares solution

# Collect predictions into data frame ----
date = seq(date_initial, date_final, by = 1)
pred_I_med = round(predictions$I)
pred_R_med = round(predictions$R)

## 95% C.I using theoretical quantiles of the poisson distribution
cl = 0.95
cl = (1 - cl) / 2
lwrI = qpois(p = cl, lambda = pred_I_med)
uprI = qpois(p = 1 - cl, lambda = pred_I_med)
pred_I=data.frame(date,pred_I_med,lwrI,uprI)

lwrR = qpois(p = cl, lambda = pred_R_med)
uprR = qpois(p = 1 - cl, lambda = pred_R_med)
pred_R=data.frame(date,pred_R_med,lwrR,uprR)

return (list(israel, pred_I, pred_R))
}

plot1 <- function(pred_I, israel){

  # Plot results ----
ci = c("#C79999")
mn = c("#7C0000")
date_breaks = "1 month"

base = ggplot() +
  xlab("") +
  scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%e %b")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  theme(legend.position = "right")

p1 = base +
  geom_line(mapping = aes(x = date, y = pred_I_med, color = colour),
            data = pred_I, size = 0.5, color = mn) +
  geom_ribbon(
    mapping = aes(x = date, ymin = lwrI, ymax = uprI),
    data = pred_I,
    size = 1, fill = ci, alpha = 0.8,
  ) +
  geom_bar(mapping = aes(x = date, y = I), stat = "identity",
           data = israel, width = 0.5, fill = 'steelblue', alpha = 0.7,
  ) +
  xlim(date_initial, date_final)

p1 = p1 + labs(y = "Active Cases")
#ggsave("Cases_8months.pdf",p1,width=8, height=6)

  return(p1)
}


plot2 <- function(pred_R, israel){
  
  ci = c("#C79999")
  mn = c("#7C0000")
  date_breaks = "1 month"
  
  base = ggplot() +
    xlab("") +
    scale_x_date(
      date_breaks = date_breaks,
      labels = scales::date_format("%e %b")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    theme(legend.position = "right")
  
  
  p2 = base +
    geom_line(mapping = aes(x = date, y = pred_R_med, color = colour),
              data = pred_R, size = 1,color=mn) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = date, ymin = lwrR, ymax=uprR),
      data =pred_R,
      size = 1,fill=ci,alpha=0.8,
    )+
    geom_bar(mapping = aes(x = date, y = R), stat = "identity",
             data = israel, width = 0.5, fill = 'steelblue', alpha = 0.7,
    ) +
    xlim(date_initial, date_final)
  p2 = p2 + labs(y = "Removed")
  #ggsave("Removed_8months.pdf",p2,width=8, height=6)
  
  return(p2)
  
}