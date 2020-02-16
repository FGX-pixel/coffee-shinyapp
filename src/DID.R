

# difference in difference

# make data table usable in DID
makeDIDdta <- function(dta, yString, controlTF, treatValuCol) {
  llength <- nrow(dta)
  didDta <- data.frame(dta[yString], 
                       c(1:llength), 
                       c(rep(as.numeric(!controlTF), llength)), 
                       treatValuCol)
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
      as.numeric(treatYN)*didinfo["treat"]*didinfo[5]
  }
  return(data.frame(days, ans))
}

# script
{
  dta3 <- selectOutletdata(outletSales, "3")
  dta5 <- selectOutletdata(outletSales, "5")
  dta8 <- selectOutletdata(outletSales, "8")
  dta3 <- makeDIDdta(dta3, "salesValue", TRUE, dta3$promoValue)
  dta5 <- makeDIDdta(dta5, "salesValue", FALSE, dta5$promoValue)
  dta8 <- makeDIDdta(dta8, "salesValue", FALSE, dta8$promoValue)
  DIDdta <- rbind.data.frame(dta3, dta5, dta8)
  
  info <- DIDinfo(DIDdta)
  ctrlG <- predictMonth(info, days, treatYN = FALSE)
  treatG <- predictMonth(info, days, treatYN = TRUE)
}


# plot
# base = control yhat
# y values = yhat5, yhat8

# diff
# y_diff = beta_t+betax*treat&promo_diff
# smoothen y_diffhat by promo_diff_t-x, calculate durable effect of params as in ma