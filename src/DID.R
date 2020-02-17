

# difference in difference

# make data table usable in DID
makeDIDdta <- function(dta, yString, treatString, controlTF, check = TRUE) {
  ldta <- dayAggValues(dta, c(yString, treatString), check = check)
  llength <- nrow(ldta)
  didDta <- data.frame(ldta[yString], 
                       c(0:(llength-1)), 
                       c(rep(as.numeric(!controlTF), llength)), 
                       ldta[treatString])
  colnames(didDta) <- c("y", "t", "treatYN", "treat")
  return(didDta)
}

# use this function to run the lm
DIDlm <- function(ldta) {
  didModel <- lm(y ~ ., data = ldta)
  return(didModel)
}
DIDinfo <- function(DIDdta) {
  linmod <- DIDlm(DIDdta)
  treatIfTreated <- DIDdta[!DIDdta$treat==0,]
  avgTreatifTreated <- mean(unlist(treatIfTreated["treat"]))
  
  ans <- c(linmod$coefficients, avgTreatifTreated)
  # colnames(ans)[1] <- "c"
  return(ans)
}
predictMonth <- function(didinfo, days, treatYN = TRUE) {
  ans <- rep(NA, length(days))
  for (i in 1:length(days)) {
    ans[i] <- didinfo["(Intercept)"] +
      i*didinfo["t"] +
      as.numeric(treatYN)*didinfo["treatYN"] +
      as.numeric(treatYN)*didinfo["treat"]*i
  }
  return(data.frame(days, ans))
}
didPlotDta <- function(control, treat) {
  ans <- treat
  ans["ans"] <- 100*(treat["ans"] - control["ans"])/control[1, "ans"]
  return(ans)
}
didBarDta <- function(didinfo, control, treat) {
  # total growth ctrl
  growthCtrl <- 100*(control[nrow(control), "ans"]-control[1, "ans"])/control[1, "ans"]
  # total growth treat
  growthTreat <- 100*(treat[nrow(treat), "ans"]-treat[1, "ans"])/treat[1, "ans"]
  # announcement/planning effect
  apEffect <- 100*didinfo["treatYN"]/treat[1, "ans"]
  # treatment effect
  tEffect <- 100*didinfo[5]/treat[1, "ans"]
  
  ans <- c(growthCtrl, growthTreat, apEffect, tEffect)
  names(ans) <- c("Default Growth", "Intervention Growth", "Planning Effect", "Intervention Effect")
  return(ans)
}

# script
{
  dta3 <- makeDIDdta(dta = outletSales, 
                     yString = "salesValue", 
                     treatString = "promoValue", 
                     controlTF = TRUE,
                     check = outletSales$sales_outlet_id=="3")
  dta5 <- makeDIDdta(dta = outletSales, 
                     yString = "salesValue", 
                     treatString = "promoValue", 
                     controlTF = FALSE,
                     check = outletSales$sales_outlet_id=="5")
  dta8 <- makeDIDdta(dta = outletSales, 
                     yString = "salesValue", 
                     treatString = "promoValue", 
                     controlTF = FALSE,
                     check = outletSales$sales_outlet_id=="8")
  DIDdta <- rbind.data.frame(dta3, dta5, dta8)
  
  info <- DIDinfo(DIDdta)
  ctrlG <- predictMonth(info, days, treatYN = FALSE)
  treatG <- predictMonth(info, days, treatYN = TRUE)
  forDidPlot <- didPlotDta(ctrlG, treatG)
  forDidBar <- didBarDta(info, ctrlG, treatG)
}


# plot
# base = control yhat
# y values = yhat5, yhat8

# diff
# y_diff = beta_t+betax*treat&promo_diff
# smoothen y_diffhat by promo_diff_t-x, calculate durable effect of params as in ma