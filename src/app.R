

## startup
{
  # run prep file
  source("prep_coffeedata.R", local = FALSE)
  # functions
  source("utils.R", local = FALSE)
  # data
  source("getTables.R", local = FALSE)
  
  # scripts
  source("getMap.R", local = FALSE)
  # dea-sim
  source("dea_sim.R", local = FALSE)
  # diff in diff
  # promo
}


## functions
{
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
  
  # points at selected outlet location
  pointAtLocation <- function(id) {
    outletInfo <- outletLocations[outletLocations$outlets.sales_outlet_id==id,]
    points(outletInfo$x, outletInfo$y, pch = 1, lwd = 20, col = "dodgerblue")
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
          names.arg = rownames(goals), 
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
