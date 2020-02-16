

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
      ## normal distributed sim
      #simValues[1,paste0(i)] <- max(0, rnorm(1, mean = pastryDist[1], sd = pastryDist[2]))
      #simValues[2,paste0(i)] <- max(0, rnorm(1, mean = promoDist[1] + (pastryDist[1]-simValues[1,paste0(i)])*truValueCorsOne[1], 
      #                                       sd = promoDist[2]))
      # min of 0.1 to avoid a divide by 0
      #simValues[3,paste0(i)] <- max(0.1, rnorm(1, mean = salesDist[1] + (pastryDist[1]-simValues[1,paste0(i)])*truValueCorsOne[2], 
      #                                         sd = salesDist[2]))
      
      # uniform distributed sim
      simValues[1,paste0(i)] <- max(0, runif(1, min = pastryDist[1] - sqrt(12)*pastryDist[2], 
                                    max = pastryDist[1] + sqrt(12)*pastryDist[2]))
      simValues[2,paste0(i)] <- max(0, runif(1, min = promoDist[1] - sqrt(12)*promoDist[2], 
                                    max = promoDist[1] + sqrt(12)*promoDist[2]) + 
                                      (pastryDist[1]-simValues[1,paste0(i)])*truValueCorsOne[1])
      # min of 0.1 to avoid a divide by 0
      simValues[3,paste0(i)] <- max(0.1, runif(1, min = salesDist[1] - sqrt(12)*salesDist[2], 
                                    max = salesDist[1] + sqrt(12)*salesDist[2]) + 
                                      (pastryDist[1] - simValues[1,paste0(i)])*truValueCorsOne[2])
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
  
  
  ## functions
  # get eff frontier
  getFrontier <- function(dtaEA, efficiencies) {
    useVertices <- t(efficiencies[,dtaEA$thetaOpt>=0.9999])
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

