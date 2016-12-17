args = commandArgs(trailingOnly=TRUE)

#projectDir = "~/documents/lu/project/"
#setwd(projectDir)

plotFormat = "pdf" # output format for plots (either jpg or pdf)

flowFile= "r1.csv"
flowMatrix = read.csv(flowFile)


minArea = 6000
maxArea = 8520
maxHeight = 0.5
disC = 0.6       # discharge coefficient - C_d
disCO = 0.64 # discharge coefficent at overflow
Qbase = 0.01 # base inflow - Q_base
nW = 3.256            # Notch Width - b
wW = 7            # Weir Width - B
bThrewB = nW / wW
zBase = Qbase / ((2/3) * disC * nW * sqrt(2 * 9.81))
Abase = 5040 * zBase + minArea
timeRange = nrow(flowMatrix)


genTable = function(notchWidth) {
  if (missing(notchWidth)) {
    notchWidth = nW
  }

  outMatrix = matrix(ncol= 6, nrow=timeRange )
  colnames(outMatrix) = c("time - s", "z - m", "q_in m^3/s", "q_out m^3/s", "A - m^2", "z + 1 - m")
  outMatrix[1,] = c(1, zBase, flowMatrix[1,2], Qbase, Abase, (zBase + ((flowMatrix[1,2] - Qbase)/Abase)))
  for ( t in 2:timeRange ) {
    outMatrix[t,1] = t                      # assign time
    outMatrix[t,2] = outMatrix[(t-1),6]     # assign z from previous z+1
    outMatrix[t,3] = flowMatrix[t,2]        # assign q_in from flowMatrix
    if (outMatrix[t,2] < (maxHeight + 0.005)) {
      outMatrix[t,4] = (2/3) * disC * notchWidth * sqrt(2 * 9.81) * outMatrix[t,2] # assign q_out
    } else {
      outMatrix[t,4] = (2/3) * disCO * wW * sqrt(2 * 9.81) * outMatrix[t,2]  # assign q_out during overflow
    }

    outMatrix[t,5] = 5040 * outMatrix[t,2] + minArea                     # assign A
    outMatrix[t,6] = outMatrix[t, 2] + ((outMatrix[t,3] - outMatrix[t,4]) / outMatrix[t,5]) # assign z + 1
  }
  return(outMatrix)
}

calcNW = function() {
  maxH = max(genTable()[,2])
  print("Calculating...")
  while (maxH < maxHeight) {
    nW = nW - 0.001 # try smaller notch widths with an increment
    maxH = max(genTable(nW)[,2])
 }
  cat("The notch width should be ", nW, "m \n")
}


genPlot = function(){
  outMatrix = genTable()
  if (plotFormat == "pdf") {
    pdf("plots.pdf")
    plot(outMatrix[,1], outMatrix[,3], type="l", col="blue", xlab="Time (s)", ylab = expression(Flow ~ m^{3} / s ~ and ~ Hight ~ m))
    lines(outMatrix[,1], outMatrix[,4], col="orange", lty=2, lwd=1.5)
    lines(outMatrix[,1], outMatrix[,2])
    legend(1, 3, c(as.expression(bquote("Inflow - " ~ Q["in"])),
                as.expression(bquote("Outflow -" ~ Q["out"])), as.expression("Water height - z")), cex=0.8, col=c("blue", "orange", "black"), lty=1:2, lwd=1:1.5)
    plot(outMatrix[,1], outMatrix[,5], type="l", xlab="Time (s)", ylab = expression(Area ~ m^{2}))
    dev.off()
  } else {
    jpeg("flow.jpg")
    plot(outMatrix[,1], outMatrix[,3], type="l", col="blue", xlab="Time (s)", ylab = expression(Flow ~ m^{3} / s))
    lines(outMatrix[,1], outMatrix[,4], col="orange", lty=2, lwd=1.5)
    legend(1, 3, c(as.expression(bquote("Inflow - " ~ Q["in"])),
                as.expression(bquote("Outflow -" ~ Q["out"])), as.expression("Water height - z")), cex=0.8, col=c("blue", "orange", "black"), lty=1:2, lwd=1:1.5)
    dev.off()
    jpeg("area.jpg")
    plot(outMatrix[,1], outMatrix[,5], type="l", xlab="Time (s)", ylab = expression(Area ~ m^{2}))
    dev.off()
  }
}


putInfo = function() {
  outMatrix = genTable()
  cat(" Max h = ", max(outMatrix[,2]), "\n", "Weir notch width = ", nW, "\n", "Max Area = ", max(outMatrix[,5]), "\n", "Max Inflow - Q_in = ", max(outMatrix[,3]), "\n", "Max Outflow - Q_out = ", max(outMatrix[,4]), "\n", "b/B = ", bThrewB, "\n")

}

aAtMidpoint = function() {
  dataTable = genTable()
  midDataTable = dataTable[c(900, 2700, 4500, 6300, 8100, 9900, 11700, 13500, 15300, 17100, 18900, 20700),]
  write.csv(midDataTable, file="mid.csv")
}

printHelp = function() {
  cat('Welcome to the Rieseberga group 3 2016 weir calculation script! \n')
  cat('You now have a number of options which you access by running this script again with an argument after the script name \n \n')
  cat('Your options are: \n')
  cat('plot     - generate a pdf or jpg plots in the cwd, format specified as variable at begninning of this script \n')
  cat('notch    - calculate a notch width that prevents overflow for the given data \n')
  cat('info     - Print information about the constants used in the plot and notch options \n')
  cat('mid      - Generate a csv file in the project/working directory with the calculated values at 30 min intervals starting at 15min \n')
  cat('help     - Show this screen \n \n')
  cat('Copyright 2016 - Tobias Ellingsen, Released under a CC0 lisence. For mor info see: \n')
  cat('https://creativecommons.org/share-your-work/public-domain/cc0/ \n')
}

if (length(args) == 1 ) {
  if (args == "plot") {
    genPlot()
  }
  else if (args == "notch") {
      nW = 3.300 # assume the notch width is less than 3.3m
      calcNW()
  }
  else if (args == "info") {
    putInfo()
  }
  else if (args == "mid"){
    aAtMidpoint()
  }
  else if (args == "help") {
    printHelp()
  }
  else {
    stop("Not a valid argument. Reffer to help for more info")
  }
} else {
  stop("Plase specify one and only one argument. For more info about which arguments are available please run this comand again with the help argument.")
}
