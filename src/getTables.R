

## prepare data tables

# load data tables
sales <- read.csv("output/April_Sales/201904 sales reciepts.csv")
# customers <- read.csv("output/April_Sales/customer.csv")
# dates <- read.csv("output/April_Sales/Dates.csv")
# generations <- read.csv("output/April_Sales/generations.csv")
outlets <- read.csv("output/April_Sales/sales_outlet.csv")
pastries <- read.csv("output/April_Sales/pastry inventory.csv")
products <- read.csv("output/April_Sales/product.csv")
targets <- read.csv("output/April_Sales/sales targets.csv")
# staff <- read.csv("output/April_Sales/staff.csv")

# join data
outletSales <- merge(x=sales, y=products, by="product_id")
pastryInfo <- merge(x=pastries, y=products, by="product_id", all.x = TRUE)

# this should not be necessary but it is (consistent form of datetime)
outletSales$transaction_date <- as.Date(outletSales$transaction_date)
pastryInfo$transaction_date <- as.Date(pastryInfo$transaction_date, "%m/%d/%Y")
pastryInfo$transaction_date <- as.Date(pastryInfo$transaction_date)


# values of interest
{
  ## create and append columns
  
  # days
  days <- unique(sales$transaction_date)
  days <- days[order(days)]
  days <- as.Date(days)
  
  # pastries
  # waste value
  wasteValue <- pastryInfo$waste*pastryInfo$current_wholesale_price
  # total pastry value
  pastriesValue <- pastryInfo$start_of_day*pastryInfo$current_wholesale_price
  
  # assemble pastries info dta
  pastryInfo <- data.frame(pastryInfo, wasteValue, pastriesValue)
  
  # sales value
  salesValue <- outletSales$quantity*outletSales$unit_price
  outletSales <- data.frame(outletSales, salesValue)
  
  # promo value
  promoValue <- outletSales$salesValue*as.numeric(outletSales$promo_item_yn=="Y")
  outletSales <- data.frame(outletSales, promoValue)
  
  
  ## aggregated values
  
  # daily sales
  daysales <- dayAggValues(outletSales, "salesValue")
  daysales <- daysales[order(daysales$days),]
  
  # waste by type
  waste <- data.frame(
    unique(pastryInfo$product_type),
    NA
  )
  for (i in 1:nrow(waste)) {
    waste[i,2] <- aggregate(pastryInfo, pastryInfo$product_type==waste[i,1], "wasteValue")
  }
  # sales by type
  typesales <- data.frame(
    unique(outletSales$product_group),
    NA
  )
  for (i in 1:nrow(typesales)) {
    typesales[i,2] <- aggregate(outletSales, outletSales$product_group==typesales[i,1], "quantity")
  }
  
  
  ## store level
  
  # targets by store
  targets358 <- targets[{
    targets$sales_outlet_id==3 | 
      targets$sales_outlet_id==5 | 
      targets$sales_outlet_id==8},
    ]
  # chain level target by product category
  goals <- data.frame(
    c(
      sum(targets358$beans_goal), 
      sum(targets358$beverage_goal), 
      sum(targets358$food_goal), 
      sum(targets358$merchandise._goal)
    ),
    row.names = c(
      "Whole Bean/Teas", 
      "Beverages", 
      "Food", 
      "Merchandise"
    )
  )
  colnames(goals) <- "target"
}


