

## functions, move to utils
{
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
}


## startup
{
  # run prep file
  source("prep_coffeedata.R", local = TRUE)
  # functions
  source("utils.R", local = TRUE)
  # data
  source("getTables.R", local = TRUE)
  
  
  ## scripts
  # map
  source("getMap.R", local = TRUE)
  # dea-sim
  source("dea_sim.R", local = TRUE)
  # diff in diff
  source("DID.R", local = TRUE)
  # promo
  source("timeline.R", local = TRUE)
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
                      "Sales in US$" = "salesValue",
                      "total quantity sold" = "quantity",
                      "Promo value in US$" = "promoValue"
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
        plotOutput("dea")
      ),
      column(
        width = 4,
        plotOutput("efficiency"), 
        # actionButton("reroll", "Reroll simulation")
      )
    ),
    fluidRow(
      verbatimTextOutput("did"),
      column(
        width = 4,
        plotOutput("didplot")
      ),
      column(
        width = 4,
        plotOutput("didbars")
      ),
      column(
        width = 4,
        "persistent effect"
      )
    ),
    fluidRow(
      verbatimTextOutput("timelineText"),
      column(
        width = 8,
        plotOutput("timeline", 
                   click = "timeline_click"
                   ),
        verbatimTextOutput("clickat")
      ),
      column(
        width = 4,
        "options", 
        checkboxGroupInput("timelineScope", 
                    label = "Please select scope of intervention", 
                    choices = list(
                      "Astoia" = "3",
                      "Lower Manhattan" = "5",
                      "Hells Kitchen" = "8"
                    ), 
                    selected = c("3", "5", "8")
        ),
        selectInput("timelineOut", 
                    label = "Please select Graph", 
                    list(
                      "Sales in US$" = "salesValue",
                      "Daily quantity sold in pcs" = "quantity", 
                      "Promo value in US$" = "promoValue", 
                      "Pastry waste in US$" = "wasteValue", 
                      "Pastries on Display in US$" = "pastriesValue"
                    )
        ),
        selectInput("timelineIn", 
                    label = "Please select Intervention", 
                    list(
                      "Promotion" = "promo",
                      "Pastries" = "pastries"
                    )
        ),
        numericInput("timelineNum", 
                     "Effort multiplier", 
                     value = 1
        )
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
        lines(daysales[,2], x = daysales[,1], col = "darkseagreen", lwd = 5)
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
    
    currentDayValues <- reactive(selectDayAggValues(outletSales, input$outletSelect, input$outletValue))
    output$outletSales <- renderPlot(
      {
        plot(currentDayValues(), main = input$outletValue, xlab = "Date", ylab = "Value", col = "darkseagreen")
        lines(currentDayValues()[,2], x = currentDayValues()[,1], col = "darkseagreen", lwd = 5)
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
    
    
    ###########
    ### DEA ###
    
    # todo intro and tutorial
    
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
    
    # observeEvent(input$reroll, dostuff)
    
    
    ###########
    ### DID ###
    
    output$did <- renderText("Difference in difference, (under construction)")
    
    output$didplot <- renderPlot(
      {
        plot(x = rep(unlist(days), 2), 
             y = c(rep(0, length(days)), unlist(forDidPlot["ans"])), 
             main = "Intervention Effect over Time", 
             xlab = "Date", 
             ylab = "% Target Value")
        lines(rep(0, nrow(ctrlG)), x = ctrlG[,1], col = "darkseagreen", lwd = 5)
        lines(forDidPlot[,"ans"], x = forDidPlot[,1], col = "dodgerblue", lwd = 5)
        legend(
          "topright",
          c("control", "intervention"),
          fill = c("darkseagreen", "dodgerblue")
        )
      }
    )
    
    output$didbars <- renderPlot(
      {
        barplot(
          main = "Effects in % on Target",
          forDidBar,
          names.arg = colnames(forDidBar),
          # las = 2,
          xpd = FALSE,
          col = "dodgerblue"
        )
      }
    )
    # todo barplots
    # todo text for persistent effect
    
    
    ################
    ### Timeline ###
    
    output$timelineText <- renderText("Timeline planning, (under construction)")
    
    # todo reactive aggregate dayvalues
    timelineTarget <- reactive(autoDayAgg(input$timelineOut, "sales_outlet_id", input$timelineScope))
    
    # todo reactive segment selection and visual feedback
    # todo reactive prognosis and ma
    
    output$timeline <- renderPlot(
      {
        plot(timelineTarget(), 
             main = "Timeline, please select Date for Intervention", 
             xlab = "Date", 
             ylab = "Value", 
             col = "darkseagreen")
        lines(timelineTarget()[,2], x = timelineTarget()[,1], col = "darkseagreen", lwd = 5)
      }
    )
    output$clickat <- renderText(
      paste("click at day ", as.Date(round(input$timeline_click$x), origin = "1970-01-01"))
    )
    
    output$end <- renderText("end")
  }
}
shinyApp(ui, server)
