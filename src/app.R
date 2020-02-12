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
  # customers <- read.csv("April_Sales/customer.csv")
  # dates <- read.csv("April_Sales/Dates.csv")
  # generations <- read.csv("April_Sales/generations.csv")
  outlets <- read.csv("April_Sales/sales_outlet.csv")
  pastries <- read.csv("April_Sales/pastry inventory.csv")
  products <- read.csv("April_Sales/product.csv")
  targets <- read.csv("April_Sales/sales targets.csv")
  # staff <- read.csv("April_Sales/staff.csv")
  
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

## extending data through random draws
{
  # get info about mean and sd
  truValues <- data.frame(getInputs("3"), getInputs("5"), getInputs("8"))
  truValues <- rbind.data.frame(truValues, c(
    aggregate(outletSales, outletSales$sales_outlet_id=="3", "quantity"), 
    aggregate(outletSales, outletSales$sales_outlet_id=="5", "quantity"), 
    aggregate(outletSales, outletSales$sales_outlet_id=="8", "quantity"))
    )
  colnames(truValues) <- c("3", "5", "8")
  # prepare simulation without disrupting means
  truValueCorsOne <- c(
    cor(as.numeric(truValues[1,]), as.numeric(truValues[2,])), 
    cor(as.numeric(truValues[1,]), as.numeric(truValues[3,]))
  )
  pastryDist <- c(mean(as.numeric(truValues[1,])), sd(truValues[1,]))
  promoDist <- c(mean(as.numeric(truValues[2,])), sd(truValues[2,]))
  salesDist <- c(mean(as.numeric(truValues[3,])), sd(truValues[3,]))
  
  # fitting a normal distribution and drawing additional input combinations
  # extend truInput by artificial observations to create 8 usable combinations
  simValues <- data.frame(matrix(nrow = 3, ncol = 8))
  colnames(simValues) <- c("3", "4", "5", "6", "7", "8", "9", "10")
  for (i in 3:10) {
    if (i %in% colnames(truValues)) {
      simValues[paste0(i)] <- truValues[paste0(i)]
    } else {
      simValues[1,paste0(i)] <- max(0, rnorm(1, mean = pastryDist[1], sd = pastryDist[2]))
      simValues[2,paste0(i)] <- max(0, rnorm(1, mean = promoDist[1] + (pastryDist[1]-simValues[1,paste0(i)])*truValueCorsOne[1], 
                                      sd = promoDist[2]))
      # min of 0.1 to avoid a divide by 0
      simValues[3,paste0(i)] <- max(0.1, rnorm(1, mean = salesDist[1] + (pastryDist[1]-simValues[1,paste0(i)])*truValueCorsOne[2], 
                                      sd = salesDist[2]))
    }
  }
  valueVec <- t(simValues)
  colnames(valueVec) <- c("Pastry expenses", "Promo value", "Quantity")
}

## DEA
library("rDEA")
{
  inps <- valueVec[,c("Pastry expenses", "Promo value")]
  outs <- valueVec[,"Quantity"]
  anoeff <- dea(
    XREF = inps,
    YREF = outs,
    X = inps, 
    Y = outs, 
    model = "input", 
    RTS =  "constant"
  )
  
  # dea plot
  simEfficiency <- data.frame(matrix(nrow = 2, ncol = 8))
  colnames(simEfficiency) <- colnames(simValues)
  for (i in colnames(simValues)) {
    simEfficiency[1,paste0(i)] <- simValues[1, i]/simValues[3, i]
    simEfficiency[2,paste0(i)] <- simValues[2, i]/simValues[3, i]
  }
  
  # get eff frontier
  getFrontier <- function(dtaEA, efficiencies) {
    useVertices <- t(efficiencies[,dtaEA$thetaOpt==1])
    useVertices <- useVertices[order(useVertices[,1]),]
    
    lineVertices <- rbind.data.frame(
      useVertices[1,]+c(0, max(efficiencies[2,])-useVertices[1,2]),
      useVertices,
      useVertices[nrow(useVertices),]+c(max(efficiencies[1,])-useVertices[nrow(useVertices),1], 0)
    )
    return(lineVertices)
  }
  deaFrontier <- getFrontier(anoeff, simEfficiency)
  
  selectEffpoint <- function(effs, id) {
    pos <- effs[id]
    points(pos[1,1], pos[2,1], pch = 1, lwd = 10, col = "black")
    points(pos[1,1], pos[2,1], pch = 1, lwd = 7, col = "red")
  }
}







# shiny
library(shiny)
{
  ui <- fluidPage(
    tags$h2("IBM's Coffee chain"),
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
        plotOutput("outletSales"),
        selectInput("outletValue", 
                    label = "Displayed data", 
                    list(
                      "Sales in US$" = "salesvalue",
                      "Quantity of consumer interactions" = "quantity"
                    )
        )
      ),
      column(
        width = 4,
        plotOutput("inputs"),
        textOutput("inputExplain")
      ),
      column(
        width = 4,
        plotOutput("efficiency"), 
        actionButton("reroll", "Reroll simulation")
      ),
      column(
        width = 4,
        plotOutput("dea")
      )
    ),
    fluidRow(
      verbatimTextOutput("promo"),
      column(
        width = 4,
        "difference in difference analysis of time series"
      )
    ),
    
    
    verbatimTextOutput("end"),
    tags$a("Project page",
           href = "https://fgx-pixel.github.io/pages/coffee_app.html"), tags$br(),
    tags$p("Based on the artificial coffe chain data by the Cognos Analytics-team at IBM")
  )
  
  
  
  
  
  
  
  
  
  server <- function(input, output, session) {
    
    #######################
    ### aggregate level ###
    
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
    
    ###################
    ### micro level ###
    
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
        par(mfrow = c(2,1))
        barplot(
          main = "Input effort",
          currentInputs()[1],
          horiz = TRUE,
          names.arg = "Pastries",
          xlim = c(max(0, min(simValues[1,])-sd(simValues[1,])), max(simValues[1,])+sd(simValues[1,])),
          xpd = FALSE,
          col = "dodgerblue"
        )
        barplot(
          currentInputs()[2],
          horiz = TRUE,
          names.arg = "Promo value",
          xlim = c(max(0, min(simValues[2,])-sd(simValues[2,])), max(simValues[2,])+sd(simValues[2,])),
          xpd = FALSE,
          col = "dodgerblue"
        )
      }
    )
    output$inputExplain <- renderText(
      "To conduct an efficiency analysis we need a quantifyable input effort. For this data the possibilities are: the amount of pastries on display to encourage a purchase of one or a related product from store or the value of the current promo campaign as a measure of input to attract customers."
    )
    
    output$efficiency <- renderPlot(
      barplot(
        anoeff$thetaOpt, 
        names.arg = c(rownames(valueVec)), 
        ylim = c(min(anoeff$thetaOpt)-.1,1),
        main = "Efficiency of Outlets",
        xpd = FALSE,
        col = "dodgerblue"
      )
    )
    
    currentEffpoint <- reactive(selectEffpoint(simEfficiency, input$outletSelect))
    output$dea <- renderPlot(
      {
        plot(
          x = c(simEfficiency[1,]),
          y = c(simEfficiency[2,]), 
          main = "DEA",
          xlab = "pastry expenses/Quantity", 
          ylab = "promo volume/Quantity",
          xlim = c(
            min(simEfficiency[1,]) - sd(simEfficiency[1,])/5,
            max(simEfficiency[1,]) + sd(simEfficiency[1,])/5
          ),
          ylim = c(
            min(simEfficiency[2,]) - sd(simEfficiency[1,])/5,
            max(simEfficiency[2,]) + sd(simEfficiency[1,])/5
          )
        )
        currentEffpoint()
        lines(x=c(deaFrontier[,1]), y=c(deaFrontier[,2]), col = "red", lwd = 5)
        text(
          x = c(simEfficiency[1,] + sd(simEfficiency[1,])/8),
          y = c(simEfficiency[2,] + sd(simEfficiency[2,])/8), 
          labels = colnames(simEfficiency)
        )
      }
    )
    
    output$end <- renderText("end")
    output$promo <- renderText("effects of a promotion")
  }
}
shinyApp(ui, server)
