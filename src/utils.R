

## functions

# aggregating general
aggregate <- function(dta, check, whatString) {
  return(sum(dta[check, whatString]))
}

dayAggValues <- function(dta, whatString) {
  days <- unique(dta$transaction_date)
  days <- days[order(days)]
  aggVals <- c(1:length(days))
  for (i in 1:length(days)) {
    # ldta <- dta[dta$transaction_date==days[i],]
    # aggVals[i] <- sum(ldta[whatString])
    aggVals[i] <- aggregate(dta, dta$transaction_date==days[i], whatString)
  }
  return(data.frame(days, aggVals))
}


getInputs <- function(outletId) {
  lpromo <- outletSales[outletSales$promo_item_yn=="Y", c("sales_outlet_id", "promoValue")]
  return(
    c(
      aggregate(pastryInfo, 
                pastryInfo$sales_outlet_id==outletId, 
                "pastriesValue"
      ), 
      aggregate(lpromo, lpromo$sales_outlet_id==outletId, "promoValue")
    )
  )
}

# selecting
selectOutletdata <- function(dta, store) {
  return(dta[dta$sales_outlet_id==store,])
}

selectDayAggValues <- function(dta, store, whatString) {
  ldta <- dayAggValues(selectOutletdata(dta, store), whatString)
  return(ldta[order(ldta$days),])
}

# points at selected outlet location
pointAtLocation <- function(id) {
  outletInfo <- outletLocations[outletLocations$outlets.sales_outlet_id==id,]
  points(outletInfo$x, outletInfo$y, pch = 1, lwd = 20, col = "dodgerblue")
}

