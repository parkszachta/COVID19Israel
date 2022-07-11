### Set Date Ranges
 
israel_SIR_preds <- function(){

source("israel_functions.R")
  
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

# Run algo for each df, using last day of previous as initial for this one 
# Add predicted values to df
# Rbind the 4 df into one
# Use geom_smooth to create lines
#     tune smoothing parameter
 

 
N=9449000
lambda = mu = 1 / (365 * 82.8) 

starting_param_val = list(beta = 1e-2, gamma = 1e-5)
estimates_pois = mle2(minuslogl = logli_SIR,
                      start = lapply(starting_param_val, log), method = "Nelder-Mead",
                      data=list(dat = df1, N = N, lambda = lambda, mu = mu))
pars_df1 = as.numeric(coef(estimates_pois))

pred_df1 = sir_1(beta = exp(pars_df1[1]), gamma = exp(pars_df1[2]), I0 = df1$I[1],
                 R0 = df1$R[1], times = df1$day, N = N, lambda = lambda,
                 mu = mu) 

df1 <- df1 %>% mutate(pred_I = pred_df1[3], pred_R = pred_df1[4])

 

 
estimates_pois = mle2(minuslogl = logli_SIR,
                      start = lapply(starting_param_val, log), method = "Nelder-Mead",
                      data=list(dat = df2, N = N, lambda = lambda, mu = mu))
pars_df2 = as.numeric(coef(estimates_pois))

pred_df2 = sir_1(beta = exp(pars_df2[1]), gamma = exp(pars_df2[2]), I0 = df2$I[1],
                 R0 = df2$R[1], times = df2$day, N = N, lambda = lambda,
                 mu = mu) 

df2 <- df2 %>% mutate(pred_I = pred_df2[3], pred_R = pred_df2[4])

 


 
estimates_pois = mle2(minuslogl = logli_SIR,
                      start = lapply(starting_param_val, log), method = "Nelder-Mead",
                      data=list(dat = df3, N = N, lambda = lambda, mu = mu))
pars_df3 = as.numeric(coef(estimates_pois))

pred_df3 = sir_1(beta = exp(pars_df3[1]), gamma = exp(pars_df3[2]), I0 = df3$I[1],
                 R0 = df3$R[1], times = df3$day, N = N, lambda = lambda,
                 mu = mu) 

df3 <- df3 %>% mutate(pred_I = pred_df3[3], pred_R = pred_df3[4])

 

 
estimates_pois = mle2(minuslogl = logli_SIR,
                      start = lapply(starting_param_val, log), method = "Nelder-Mead",
                      data=list(dat = df4, N = N, lambda = lambda, mu = mu))
pars_df4 = as.numeric(coef(estimates_pois))

pred_df4 = sir_1(beta = exp(pars_df4[1]), gamma = exp(pars_df4[2]), I0 = df4$I[1],
                 R0 = df4$R[1], times = df4$day, N = N, lambda = lambda,
                 mu = mu) 

df4 <- df4 %>% mutate(pred_I = pred_df4[3], pred_R = pred_df4[4])

 

 
israel_new <- rbind(df1, df2, df3, df4)
return(israel_new)
}

SIR_plot1 <- function(israel_new, date_initial, date_final){
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
  geom_smooth(mapping = aes(x = date, y = pred_I$I, color = colour),
              data = israel_new, size = 1,color = mn) +
  geom_bar(mapping = aes(x = date, y = I), stat = "identity",
           data = israel_new, width = 0.5, fill = 'steelblue', alpha = 0.7,
  ) +
  xlim(date_initial, date_final)
p1 = p1 + labs(y = "Active Infected")
 
return (p1)
}

SIR_plot2 <- function(israel_new, date_initial, date_final){
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
  geom_line(mapping = aes(x = date, y = pred_R$R, color = colour),
            data = israel_new, size = 1,color = mn) +
  geom_bar(mapping = aes(x = date, y = R), stat = "identity",
           data = israel_new, width = 0.5, fill = 'steelblue', alpha = 0.7,
  ) +
  xlim(date_initial, date_final)
p2 = p2 + labs(y = "Removed")

 
}

