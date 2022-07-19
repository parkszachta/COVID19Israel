source("israel_functions.R")
library(deSolve)
library(outbreaks)
library(gridExtra)
library(arm)
library(tidyverse)
library(bbmle)


predict_data = function(df, params){
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma = 1 / (5.8)
  
  ret_val <- SEIR_k_optim_SMAPE(df, params)
  k <- ret_val[[1]]
  estimates_pois <- ret_val[[2]]
  
  pars_df = estimates_pois
  
  pred_df = seir_1(beta = exp(pars_df[1]), gamma = exp(pars_df[2]),
                   sigma = sigma, I0 = df$I[1], R0 = df$R[1], K = k,
                   times = df$day, N = N, lambda = lambda,
                   mu = mu)
  
  # df <- df %>% mutate(pred_I = pred_df[4], pred_R = pred_df[5])
  df <- cbind(df, pred_df[4], pred_df[5])
  df %>%
    rename(pred_I = 11,
           pred_R = 12)
  
  return(list(df, pars_df, k))
}

ciband<-function(sigma_l, sigma_u, sigma_m, beta, gamma, data, rep, K){
  pred_I_med = data[11]
  pred_R_med = data[12]
  lwrI = round(apply(pred_I_med,1,quantile, probs=0.025))
  uprI = round(apply(pred_I_med,1,quantile, probs=0.975))
  sd=(uprI-lwrI)/1.96*2
  lwrI=pred_I_med-1.96*sd
  uprI=pred_I_med+1.96*sd
  pred_I=data.frame(data$date,pred_I_med,lwrI,uprI)
  lwrR = round(apply(pred_R_med,1,quantile, probs=0.025))
  uprR = round(apply(pred_R_med,1,quantile, probs=0.975))
  sd=(uprR-lwrR)/1.96*2
  lwrR=pred_R_med-1.96*sd
  uprR=pred_R_med+1.96*sd
  df <- cbind(data, lwrI, uprI, lwrR, uprR)
  df <- df %>%
    rename(pred_I_med= 11,
           pred_R_med = 12,
           lwrI = 13,
           uprI = 14,
           lwrR = 15,
           uprR = 16)
  return (df)
}


israel_pred_df = function(){
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
  df1 <- israel %>% filter(period == 1)
  df2 <- israel %>% filter(period == 2)
  df3 <- israel %>% filter(period == 3)
  df4 <- israel %>% filter(period == 4)
  
  N=9449000
  lambda = mu = 1 / (365 * 82.8)
  sigma_m = 1 / 5.5
  sigma_l = 1 / 5.9
  sigma_u = 1/ 5.1
  rep = 100
  
  starting_param_val = log(c(1e-2,1e-5))
  
  a = predict_data(df1, starting_param_val)
  df1 = as.data.frame(a[[1]])
  df1_params <- a[[2]]
  k = a[[3]]
  df1 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df1_params[1], gamma = df1_params[2], data = df1, rep, K = k)
  
  
  
  
  a = predict_data(df2, df1_params)
  df2 = as.data.frame(a[[1]])
  df2_params <- a[[2]]
  k = a[[3]]
  df2 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df2_params[1], gamma = df2_params[2], df2, rep, K = k)
  
  a = predict_data(df3, df2_params)
  df3 = as.data.frame(a[[1]])
  df3_params <- a[[2]]
  k = a[[3]]
  df3 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df3_params[1], gamma = df3_params[2], df3, rep, K = k)
  
  
  a = predict_data(df4, df3_params)
  df4 = as.data.frame(a[[1]])
  df4_params <- a[[2]]
  k = a[[3]]
  df4 <- ciband(sigma_l = sigma_l, sigma_m = sigma_m, sigma_u = sigma_u,
                beta = df4_params[1], gamma = df4_params[2], df4, rep, K = k)
  
  israel_new <- rbind.data.frame(df1, df2, df3, df4)
  
  return(israel_new)
}


SEIR_plot1 <- function(israel_new){
  
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
    geom_smooth(mapping = aes(x = date, y = pred_I_med, color = colour),
                data = israel_new, size = 0.5, color = mn) +
    geom_ribbon(
      mapping = aes(x = date, ymin = lwrI, ymax = uprI),
      data = israel_new,
      size = 1, fill = ci, alpha = 0.8,
    ) +
    geom_bar(mapping = aes(x = date, y = I), stat = "identity",
             data = israel_new, width = 0.5, fill = 'steelblue', alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  
  p1 = p1 + labs(y = "Active Cases")
  #ggsave("Cases_8months.pdf",p1,width=8, height=6)
  
  return(p1)
}


SEIR_plot2 <- function(israel_new){
  
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
              data = israel_new, size = 1,color=mn) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(x = date, ymin = lwrR, ymax=uprR),
      data = israel_new,
      size = 1,fill=ci,alpha=0.8,
    )+
    geom_bar(mapping = aes(x = date, y = R), stat = "identity",
             data = israel_new, width = 0.5, fill = 'steelblue', alpha = 0.7,
    ) +
    xlim(israel_new$date[1], israel_new$date[nrow(israel_new)])
  p2 = p2 + labs(y = "Removed")
  #ggsave("Removed_8months.pdf",p2,width=8, height=6)
  
  return(p2)
  
}
