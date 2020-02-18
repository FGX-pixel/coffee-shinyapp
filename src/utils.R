

## functions

# aggregating general
aggregate <- function(dta, check = TRUE, whatString) {
  return(sum(dta[check, whatString]))
}
dayAggregate <- function(dta, whatString, baseDta = dta) {
  ans <- 1:length(days)
  for (i in 1:length(days)) {
    # dont use, HUGE hit on performance!!!
    # ans[i] <- aggregate(dta, as.Date(baseDta$transaction_date)==as.Date(days[i]), whatString)
    
    ans[i] <- aggregate(dta, baseDta$transaction_date==days[i], whatString)
  }
  return(ans)
}

## aggregate higher
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

# aggAutoSelect
autoSelectDta <- function(whatString) {
  if (whatString %in% colnames(pastryInfo)) {
    return(pastryInfo)
  }
  if (whatString %in% colnames(outletSales)) {
    return(outletSales)
  }
  return(NULL)
}
# autoDayAgg
autoDayAgg <- function(whatString, checkColString = "sales_outlet_id", checkEquals = c("3")) {
  ldta <- autoSelectDta(whatString)
  lcheckCol <- ldta[checkColString]
  lcheck <- sapply(lcheckCol, "%in%", checkEquals)
  
  return(dayAggValues(ldta, whatString, lcheck))
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

