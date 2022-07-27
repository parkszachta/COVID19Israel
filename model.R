model <- function() {
  library(EpiDynamics)
  options(scipen=999)
  source("positive_rate.R")
  source("estimate_tvr_israel.R")
  source("SEIR_SMAPEvsSSE.R")
  source("eSIR.R")
  
  israel <- as.data.frame(israel_pred_df())
  israel_SIR <- as.data.frame(pred_SIR())
  israel_eSIR_plots <- get_eSIR_plots()

  israel$pred_I_med = unlist(israel$pred_I_med)
  israel$pred_R_med = unlist(israel$pred_R_med)
  israel$date = unlist(israel$date)
  israel$lwrI = unlist(israel$lwrI)
  israel$lwrR = unlist(israel$lwrR)
  israel$uprI = unlist(israel$uprI)
  israel$uprR = unlist(israel$uprR)
  
  israel_SIR$pred_I = unlist(israel_SIR$pred_I)
  israel_SIR$pred_R = unlist(israel_SIR$pred_R)
  israel_SIR$date = unlist(israel_SIR$date)


  SEIRp1 <- SEIR_plot1(israel)
  SEIRp2 <- SEIR_plot2(israel)
  SIRp1 <- SIR_plot1(israel_SIR)
  SIRp2 <- SIR_plot2(israel_SIR)
  tpr <- positive_rate()
  tvr <- tvr_plot()
  eSIRp1 <- israel_eSIR_plots[[1]]
  eSIRp2 <- israel_eSIR_plots[[2]]
  
  return(list(SEIRp1 = SEIRp1, SEIRp2 = SEIRp2, tpr = tpr, tvr = tvr, SIRp1 = SIRp1, SIRp2 = SIRp2, eSIRp1 = eSIRp1, eSIRp2 = eSIRp2))

}

results = model()

save(results, file="output/model_results.RData")
