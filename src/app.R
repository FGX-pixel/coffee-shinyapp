# run prep file
source("prep_coffeedata.R", local = FALSE)


# functions
{
  # aggregating
  aggregate <- function(dta, check, whatString) {
    ldta <- dta[check,]
    return(sum(ldta[whatString]))
  }
  
  dayAggValues <- function(dta, whatString) {
    days <- unique(dta$transaction_date)
    days <- days[order(days)]
    aggVals <- c(1:length(days))
    for (i in 1:length(days)) {
      ldta <- dta[dta$transaction_date==days[i],]
      aggVals[i] <- sum(ldta[whatString])
    }
    return(data.frame(days, aggVals))
  }
  
  getInputs <- function(outletId) {
    lpromo <- outletSales[outletSales$promo_item_yn=="Y",]
    return(
      c(
        aggregate(pastryInfo, pastryInfo$sales_outlet_id==outletId, "pastriesValue"), 
        aggregate(lpromo, pastryInfo$sales_outlet_id==outletId, "salesValue")
      )
    )
  }
  
  # selecting
  selectOutletdata <- function(dta, inp) {
    return(dta[dta$sales_outlet_id==inp,])
  }
  
  selectDayAggValues <- function(dta, inp, whatString) {
    ldta <- dayAggValues(selectOutletdata(dta, inp), whatString)
    return(ldta[order(ldta$days),])
  }
}


# load tables
{
  sales <- read.csv("April_Sales/201904 sales reciepts.csv")
  #customers <- read.csv("April_Sales/customer.csv")
  #dates <- read.csv("April_Sales/Dates.csv")
  #generations <- read.csv("April_Sales/generations.csv")
  outlets <- read.csv("April_Sales/sales_outlet.csv")
  pastries <- read.csv("April_Sales/pastry inventory.csv")
  products <- read.csv("April_Sales/product.csv")
  targets <- read.csv("April_Sales/sales targets.csv")
  staff <- read.csv("April_Sales/staff.csv")
  
  # useable data
  outletSales <- merge(x=sales, y=products, by="product_id")
  salesValue <- outletSales$quantity*outletSales$unit_price
  outletSales <- data.frame(outletSales, salesValue)
  pastryInfo <- merge(x=pastries, y=products, by="product_id", all.x = TRUE)
  wasteValue <- pastryInfo$waste*pastryInfo$current_wholesale_price
  pastriesValue <- pastryInfo$start_of_day*pastryInfo$current_wholesale_price
  pastryInfo <- data.frame(pastryInfo, wasteValue, pastriesValue)
  
  ## constant aggvals
  # sales
  daysales <- dayAggValues(outletSales, "salesValue")
  daysales <- daysales[order(daysales$days),]
  # waste
  waste <- data.frame(
    unique(pastryInfo$product_type),
    NA
  )
  for (i in 1:nrow(waste)) {
    waste[i,2] <- aggregate(pastryInfo, pastryInfo$product_type==waste[i,1], "wasteValue")
  }
  #sales by type
  typesales <- data.frame(
    unique(outletSales$product_group),
    NA
  )
  for (i in 1:nrow(typesales)) {
    typesales[i,2] <- aggregate(outletSales, outletSales$product_group==typesales[i,1], "quantity")
  }
  # targets
  targets358 <- targets[{
    targets$sales_outlet_id==3 | 
      targets$sales_outlet_id==5 | 
      targets$sales_outlet_id==8},
    ]
  goals <- data.frame(
    c(
      sum(targets358$beans_goal), 
      sum(targets358$beverage_goal), 
      sum(targets358$food_goal), 
      sum(targets358$merchandise._goal)
    )
  )
}

## map
{
  library(OpenStreetMap)
  # store locations
  outletLocations <- as.data.frame(projectMercator( lat = outlets$store_latitude, long = outlets$store_longitude))
  outletLocations <- data.frame(outlets$sales_outlet_id, outletLocations)
  outletLocations <- outletLocations[outlets$sales_outlet_type=="retail",]
  
  # dynamic map selection
  mapMargin <- .05
  mapCornerUL <- c(max(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) + mapMargin, 
                   min(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) - mapMargin)
  mapCornerDR <- c(min(outlets[outlets$sales_outlet_type=="retail",]$store_latitude) - mapMargin,
                   max(outlets[outlets$sales_outlet_type=="retail",]$store_longitude) + mapMargin)
  nycMap <- openmap(mapCornerUL, mapCornerDR, type = "osm")
  # selected outlet location
  pointAtLocation <- function(id) {
    outletInfo <- outletLocations[outletLocations$outlets.sales_outlet_id==id,]
    points(outletInfo$x, outletInfo$y, pch = 1, lwd = 20, col = "dodgerblue")
  }
}




## DEA
#library("rDEA")








# shiny
library(shiny)
{
  ui <- fluidPage(
    tags$h2("IBM's Coffe chain"),
    fluidRow(
      verbatimTextOutput("macro"),
      column(
        width = 4,
        plotOutput("aggsales")
      ),
      column(
        width = 4,
        plotOutput("aggwaste")
      ),
      column(
        width = 4,
        plotOutput("typesales")
      )
    ),
    fluidRow(
      verbatimTextOutput("micro"),
      column(
        width = 4,
        plotOutput("map"),
        selectInput("outletSelect", 
                    label = "Select store",
                    list(
                      "Store 3, Astoia" = "3",
                      "Store 5, Lower Manhattan" = "5",
                      "Store 8, Hell's Kitchen" = "8"
                    )
        )
      ),
      column(
        width = 4,
        plotOutput("outletSales")
      ),
      column(
        width = 4,
        plotOutput("inputs"),
        textOutput("inputExplain")
      ),
      column(
        width = 4,
        "DEA efficiency"
      ),
      column(
        width = 4,
        "efficiency analysis"
      )
    ),
    fluidRow(
      verbatimTextOutput("promo"),
      column(
        width = 4,
        "sale of items that received a promo"
      )
    ),
    tags$p("Based on the artificial coffe chain data by the Cognos Analytics-team at IBM")
  )
  
  server <- function(input, output, session) {
    output$macro <- renderText("Chain level")
    output$aggsales <- renderPlot(
      {
        plot(daysales, main = "Sales", xlab = "Date", ylab = "Sales in US$", col = "darkseagreen")
        lines(daysales[,2], col = "darkseagreen", lwd = 5)
      }
    )
    output$aggwaste <- renderPlot(
      {
        barplot(waste[,2], names.arg = waste[,1], main = "Spoilage", col = "darkorange")
      }
    )
    output$typesales <- renderPlot(
      {
        barplot(
          rbind(c(typesales[-3,2]), c(goals[,1])), 
          names.arg = typesales[-3,1], 
          main = "Sales by type against target", 
          col = c("darkseagreen", "dodgerblue"),
          beside = TRUE
        )
        legend(
          "topright",
          c("Sales", "Goal"),
          fill = c("darkseagreen", "dodgerblue")
        )
      }
    )
    
    
    output$micro <- renderText("Outlet level")
    
    pointSelected <- reactive(pointAtLocation(input$outletSelect))
    output$map <- renderPlot(
      {
        plot(nycMap)
        # selected outlet marker
        pointSelected()
        # regular markers
        points(outletLocations$x, outletLocations$y, pch = 1, lwd = 10, col = "black")
        points(outletLocations$x, outletLocations$y, pch = 1, lwd = 6, col = "chocolate")
      }
    )
    
    currentDayValues <- reactive(selectDayAggValues(outletSales, input$outletSelect, "salesValue"))
    output$outletSales <- renderPlot(
      {
        plot(currentDayValues(), main = "Sales", xlab = "Date", ylab = "Sales in US$", col = "darkseagreen")
        lines(currentDayValues()[,2], col = "darkseagreen", lwd = 5)
      }
    )
    
    currentInputs <- reactive(getInputs(input$outletSelect))
    output$inputs <- renderPlot(
      {
        barplot(
          currentInputs(), 
          names.arg = c("Pastries", "Promo value"), 
          main = paste("Inputs for store: ", input$outletSelect), 
          col = "dodgerblue"
        )
      }
    )
    output$inputExplain <- renderText(
      "To conduct an efficiency analysis we need a quantifyable input effort. For this data the possibilities are: the amount of pastries on display to encourage a purchase of one or a related product from store, the number of employees or the value of the current promo campaign as a measure of input to attract customers."
    )
    
    
    output$promo <- renderText("effects of a promotion")
  }
}
shinyApp(ui, server)
