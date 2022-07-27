source("israel_functions.R")
library(deSolve)
library(outbreaks)
library(gridExtra)
library(arm)
library(tidyverse)
library(bbmle)
library(plotly)

predict_data_SMAPE = function(df, params){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma = 1 / (5.8)
  
  ret_val <- SEIR_k_optim(df, params)
  k <- ret_val[[1]]
  estimates_pois <- ret_val[[2]]
  
  pars_df = estimates_pois
  
  pred_df = seir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   sigma = sigma, I0 = df$I[1], R0 = df$R[1], K = k,
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  new_df <- data.frame(day = df$day, pred_df[4], pred_df[5])
  new_df <- new_df %>%
    rename(pred_I = I,
           pred_R = R)
  df <- cbind(df, new_df)
  return(list(df, pars_df, k))
}


predict_data_test <- function(df, pars_df, k){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma = 1 / (5.8)
  pred_df = seir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   sigma = sigma, I0 = df$I[1], R0 = df$R[1], K = k,
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  new_df <- data.frame(day = df$day, pred_df[4], pred_df[5])
  new_df <- new_df %>%
    rename(pred_I = I,
           pred_R = R)
  df <- cbind(df, new_df)
  return(df)
}

predict_data_SIR = function(df, params){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)

  ret_val <- optim(params, SMAPE2_SIR, N = N, 
                   data = df, lambda = lambda, mu = mu)

  pars_df <- ret_val[[1]]
  
  pred_df = sir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   I0 = df$I[1], R0 = df$R[1],
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  new_df <- data.frame(day = df$day, pred_df[3], pred_df[4])
  new_df <- new_df %>%
    rename(pred_I = I,
           pred_R = R)
  df <- cbind(df, new_df)
  return(list(df, pars_df))
}

predict_data_testSIR <- function(df, pars_df){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma = 1 / (5.8)
  pred_df = sir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   I0 = df$I[1], R0 = df$R[1],
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  new_df <- data.frame(day = df$day, pred_df[3], pred_df[4])
  new_df <- new_df %>%
    rename(pred_I = I,
           pred_R = R)
  df <- cbind(df, new_df)
  return(df)
}

predict_data_SSE = function(df, params){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma = 1 / (5.8)
  
  ret_val <- SEIR_k_optim(df, params)
  k <- ret_val[[1]]
  estimates_pois <- ret_val[[2]]
  
  pars_df = estimates_pois
  view(df)
  pred_df = seir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   sigma = sigma, I0 = df$I[1], R0 = df$R[1], K = k,
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  
  new_df <- data.frame(pred_I_SSE = pred_df[4], pred_R_SSE = pred_df[5])
  
  
  return(list(new_df, pars_df, k))
}

ciband<-function(sigma_l, sigma_u, sigma_m, beta, gamma, data, rep, K, N, lambda, mu){
  pred_I_med = data$pred_I
  pred_R_med = data$pred_R
  sd = (1/sigma_l - 1/sigma_u) / 1.96 * 2
  pred_I=matrix(0,nrow=nrow(data),ncol=rep)
  pred_R=matrix(0,nrow=nrow(data),ncol=rep)
  for(i in 1:rep){
    De=rnorm(1,mean=1/sigma_m, sd=sd)
    sigma=1/De
    predictions = seir_1(beta = beta, gamma = gamma, I0 = data$I[1],
                         R0 = data$R[1], times = data$day, N = N, lambda = lambda,
                         mu = mu, sigma=sigma, K = K)
    p_I=rpois(nrow(data),lambda = data$pred_I)
    p_R=rpois(nrow(data),lambda = data$pred_R)
    pred_I[,i]=p_I
    pred_R[,i]=p_R
  }
  lwrI = round(apply(pred_I,1,quantile, probs=0.025))
  uprI = round(apply(pred_I,1,quantile, probs=0.975))
  sd=(uprI-lwrI)/1.96*2
  lwrI=pred_I_med-1.96*sd
  uprI=pred_I_med+1.96*sd
  pred_I=data.frame(data$day,lwrI,uprI)
  lwrR = round(apply(pred_R,1,quantile, probs=0.025))
  uprR = round(apply(pred_R,1,quantile, probs=0.975))
  sd=(uprR-lwrR)/1.96*2
  lwrR=pred_R_med-1.96*sd
  uprR=pred_R_med+1.96*sd
  pred_R=data.frame(data$day,lwrR,uprR)
  
  df <- cbind(data, pred_I, pred_R)
  return (df)
}

israel_pred_df = function(){
  date0 <- as.Date("2020-12-27")
  date1 <- as.Date("2021-01-07")
  date2 <- as.Date("2021-02-07")
  date3 <- as.Date("2021-03-07")
  date4 <- as.Date("2021-04-18")
  test_date <- as.Date("2021-05-01")

  
  date_initial = date0
  date_final = test_date
  
  israel <- israel_data_only(date_initial, date_final)
  
  israel = israel %>%
    mutate(period = ifelse(date < date1, 1,
                           ifelse(date >= date1 & date < date2, 2,
                                  ifelse(date >= date2 & date < date3, 3,
                                         ifelse(date >= date3 & date < date4, 4, 
                                                ifelse(date >= date4, 5, 0))))))

  df1 <- israel %>% filter(period == 1)
  df2 <- israel %>% filter(period == 2)
  df3 <- israel %>% filter(period == 3)
  df4 <- israel %>% filter(period == 4)
  test_df <- israel %>% filter(period == 5)
  
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma_m = 1 / 5.5
  sigma_l = 1 / 5.9
  sigma_u = 1/ 5.1
  rep = 100
  
  starting_param_val = log(c(1e-2,1e-5))
  a = predict_data_SMAPE(df1, starting_param_val)
  df1 = as.data.frame(a[[1]])
  df1_params <- a[[2]]
  k = a[[3]]
  df1 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df1_params[1], gamma = df1_params[2], data = df1, rep, K = k, N = N, lambda = lambda, mu = mu)
  
  a = predict_data_SMAPE(df2, df1_params)
  df2 = as.data.frame(a[[1]])
  df2_params <- a[[2]]
  k = a[[3]]
  df2 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df2_params[1], gamma = df2_params[2], df2, rep, K = k, N = N, lambda = lambda, mu = mu)
  
  a = predict_data_SMAPE(df3, df2_params)
  df3 = as.data.frame(a[[1]])
  df3_params <- a[[2]]
  k = a[[3]]
  df3 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df3_params[1], gamma = df3_params[2], df3, rep, K = k, N = N, lambda = lambda, mu = mu)
  
  
  a = predict_data_SMAPE(df4, df3_params)
  df4 = as.data.frame(a[[1]])
  df4_params <- a[[2]]
  k = a[[3]]
  df4 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df4_params[1], gamma = df4_params[2], df4, rep, K = k, N = N, lambda = lambda, mu = mu)
  
  a = predict_data_test(test_df, df4_params, k)
  test_df <- as.data.frame(a)
  test_df <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df4_params[1], gamma = df4_params[2], test_df, rep, K = k, N = N, lambda = lambda, mu = mu)
  
  israel_new <- rbind.data.frame(df1, df2, df3, df4, test_df)
  
  return(israel_new)
}


SEIR_plot1 <- function(israel_new){
  ci = c("#C79999")
  mn = c("#7C0000", "#000000")
  train_pred_color <- c("#D55E00")
  test_pred_color <- c("#F0E442")
  date_breaks = "1 month"
  israel_new <- israel_new[-11]
  
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
    geom_line(mapping = aes(x = date, y = pred_I, color = ifelse(period == 5, train_pred_color, test_pred_color)),
              data = israel_new, size = 1) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = date, ymin = lwrI, ymax=uprI, color = ifelse(period == 5, train_pred_color, test_pred_color)),
      data = israel_new,
      size = 1, alpha=0.2,
    )+
    geom_bar(mapping = aes(x = date, y = I), stat = "identity",
             data = israel_new, width = 0.5, fill = "#009E73", alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  p1 = p1 + labs(y = "Active Cases")
  p1 = ggplotly(p1)
  

  return(p1)
}


SEIR_plot2 <- function(israel_new){
  ci = c("#C79999")
  mn = c("#7C0000")
  date_breaks = "1 month"
  israel_new <- israel_new[-11]
  
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

  train_pred_color <- c("#D55E00")
  test_pred_color <- c("#F0E442")
  p2 = base +
    geom_line(mapping = aes(x = date, y = pred_R, color = ifelse(period == 5, train_pred_color, test_pred_color)),
              data = israel_new, size = 1) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = date, ymin = lwrR, ymax=uprR, color = ifelse(period == 5, train_pred_color, test_pred_color)),
      data = israel_new,
      size = 1, alpha=0.8,
    )+
    geom_bar(mapping = aes(x = date, y = R), stat = "identity",
             data = israel_new, width = 0.5, fill = "#009E73", alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  p2 = p2 + labs(y = "Total Removed")

  
  p2 = ggplotly(p2)
  
  
  #ggsave("Removed_8months.pdf",p2,width=8, height=6)
  
  
  # israel_new <- israel_new[-11]
  # israel_new = israel_new %>%
  #   mutate(train_test = ifelse(period == 5, "test", "train"))
  # 
  # p2 = plot_ly(data = israel_new, x = ~date, y=~R, type = 'bar', name = 'Observed Removed') %>%
  #   
  #   add_trace(data = israel_new, x = ~date, y = ~pred_R, type='scatter', 
  #             mode = 'lines', name = 'Removed Model', line = list(width = 4))  %>%
  #   
  #   add_ribbons(data = israel_new, x = ~date, ymin = ~lwrR, ymax = ~uprR, name = "CI Band", opacity = 0.8)
  # 
  # p2 <- p2 %>% layout(yaxis = list(title = "Cumulative Removed"))
  
  return(p2)
  
}

SIR_plot1 <- function(israel_new){
  ci = c("#C79999")
  mn = c("#7C0000", "#000000")
  train_pred_color <- c("#D55E00")
  test_pred_color <- c("#F0E442")
  date_breaks = "1 month"
  israel_new <- israel_new[-11]
  
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
    geom_line(mapping = aes(x = date, y = pred_I, color = ifelse(period == 5, train_pred_color, test_pred_color)),
              data = israel_new, size = 1) +
    geom_bar(mapping = aes(x = date, y = I), stat = "identity",
             data = israel_new, width = 0.5, fill = "#009E73", alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  p1 = p1 + labs(y = "Active Cases")
  p1 = ggplotly(p1)
  
  
  return(p1)
}

SIR_plot2 <- function(israel_new){
  ci = c("#C79999")
  mn = c("#7C0000", "#000000")
  train_pred_color <- c("#D55E00")
  test_pred_color <- c("#F0E442")
  date_breaks = "1 month"
  israel_new <- israel_new[-11]
  
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
    geom_line(mapping = aes(x = date, y = pred_R, color = ifelse(period == 5, train_pred_color, test_pred_color)),
              data = israel_new, size = 1) +
    geom_bar(mapping = aes(x = date, y = R), stat = "identity",
             data = israel_new, width = 0.5, fill = "#009E73", alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  p2 = p2 + labs(y = "Total Removed")
  p2 = ggplotly(p2)
  
  
  return(p2)
}

pred_SIR = function(){
  date0 <- as.Date("2020-12-27")
  date1 <- as.Date("2021-01-07")
  date2 <- as.Date("2021-02-07")
  date3 <- as.Date("2021-03-07")
  date4 <- as.Date("2021-04-18")
  test_date <- as.Date("2021-05-01")
  
  
  date_initial = date0
  date_final = test_date
  
  israel <- israel_data_only(date_initial, date_final)
  
  israel = israel %>%
    mutate(period = ifelse(date < date1, 1,
                           ifelse(date >= date1 & date < date2, 2,
                                  ifelse(date >= date2 & date < date3, 3,
                                         ifelse(date >= date3 & date < date4, 4, 
                                                ifelse(date >= date4, 5, 0))))))
  
  df1 <- israel %>% filter(period == 1)
  df2 <- israel %>% filter(period == 2)
  df3 <- israel %>% filter(period == 3)
  df4 <- israel %>% filter(period == 4)
  test_df <- israel %>% filter(period == 5)
  
  N=9449000
  lambda = mu = 1 / (365 * 82.8)

  rep = 100
  
  starting_param_val = log(c(1e-2,1e-5))
  a = predict_data_SIR(df1, starting_param_val)
  df1 = as.data.frame(a[[1]])
  df1_params <- a[[2]]

  a = predict_data_SIR(df2, df1_params)
  df2 = as.data.frame(a[[1]])
  df2_params <- a[[2]]
  
  a = predict_data_SIR(df3, df2_params)
  df3 = as.data.frame(a[[1]])
  df3_params <- a[[2]]

  a = predict_data_SIR(df4, df3_params)
  df4 = as.data.frame(a[[1]])
  df4_params <- a[[2]]

  a = predict_data_testSIR(test_df, df4_params)
  test_df <- as.data.frame(a)

  israel_new <- rbind.data.frame(df1, df2, df3, df4, test_df)
  
  return(israel_new)
}
