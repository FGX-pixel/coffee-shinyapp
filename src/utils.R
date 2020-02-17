

## functions

# aggregating general
aggregate <- function(dta, check = TRUE, whatString) {
  return(sum(dta[check, whatString]))
}
dayAggregate <- function(dta, whatString, baseDta = dta) {
  ans <- 1:length(days)
  for (i in 1:length(days)) {
    ans[i] <- aggregate(dta, baseDta$transaction_date==days[i], whatString)
  }
  return(ans)
}

dayAggValues <- function(dta, whatStrings, check = TRUE) {
  ldta <- dta[check,]
  ans <- matrix(nrow = length(days), ncol = length(whatStrings))
  
  valueDta <- sapply(whatStrings, selectValues, dta = ldta)
  valueDta <- sapply(valueDta, rbind)
  colnames(valueDta) <- whatStrings
  for (i in 1:ncol(valueDta)) {
    ans[,i] <- dayAggregate(valueDta, whatString = whatStrings[i], baseDta = ldta)
  }
  colnames(ans) <- whatStrings
  return(data.frame(days, ans))
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
  # replace with selectValues
  return(dta[dta$sales_outlet_id==store,])
}
selectValues <- function(dta, whatString, check = TRUE) {
  ldta <- dta[check,]
  return(ldta[whatString])
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

