

## functions

# aggregating general
aggregate <- function(dta, check, whatString) {
  ldta <- dta[check,]
  return(sum(ldta[whatString]))
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