#!/usr/bin/env Rscript

# Start this program via @./em.R@ (in the directory of the program)

# TODO modify sourceCpp to includes the full directory we are in
# TODO consider using rPackages.littler

library (colortools)
library (flowCore)
library (flowViz)
library (ggplot2)
library (gtools)
library (mixtools)
library (mvtnorm)
library (optparse)
library (parallel)
library (Rcpp)
library (tictoc)

#source("classes.R")
source("plotting.R")
#source("writestats.R")
#source("emwrapper.R")
source("./R/flowemmi.R")

parser <- OptionParser ()
parser <- add_option (parser, c ("-v", "--verbose"), action = "store_true", default=FALSE, help="be very verbose")
parser <- add_option (parser, c ("-f", "--file"), type="character", default = "", help="file to process")
parser <- add_option (parser, c ("-x", "--channelx"), type="character", default = "PMT.1", help="first channel")
parser <- add_option (parser, c ("-y", "--channely"), type="character", default = "PMT.9", help="second channel")
parser <- add_option (parser, c ("--xstart"), type="integer", default = 1500, help="")
parser <- add_option (parser, c ("--xend"), type="integer", default = 40000, help="")
parser <- add_option (parser, c ("--ystart"), type="integer", default = 5000, help="")
parser <- add_option (parser, c ("--yend"), type="integer", default = 38000, help="")
parser <- add_option (parser, c ("--convergence"), type="double", default = 0.01, help="")
parser <- add_option (parser, c ("--initfraction"), type="double", default = 0.01, help="")
parser <- add_option (parser, c ("--finalfraction"), type="double", default = 1.0, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--mincluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--maxcluster"), type="integer", default = 20, help="")
parser <- add_option (parser, c ("--clusterbracket"), type="integer", default = 3, help="")
parser <- add_option (parser, c ("--parallel"), action="store_true", default = FALSE, help="")

opts <- parse_args(parser)





# load sample
if (opts$file != "") {
  fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
  print (colnames(fcsData))
  fdo <- mkFlowDataObject(fcsData, xChannel=opts$channelx, yChannel=opts$channely)
  # run actual flowEMMi algorithm
  results <- flowEMMi( fdo=fdo
                     , xMin = opts$xstart, xMax = opts$xend, yMin=opts$ystart, yMax=opts$yend
                     , initFraction = opts$initfraction
                     , finalFraction = opts$finalfraction
                     , numberOfInits = opts$inits
                     , useLogScale = opts$log
                     , imageFormat = opts$imgformat
                     , minClusters = opts$mincluster, maxClusters = opts$maxcluster, clusterbracket=opts$clusterbracket
                     , parallel = opts$parallel
                     , convergenceEpsilon = opts$convergence
                     , verbose = opts$verbose
                     )
  # plot the results
  png("resultOfFlowEMMi.png")
  plotDensityAndEllipsesByRelevance(data = fcsData@exprs[,c(opts$channelx,opts$channely)], results = results$best)
  dev.off()
}
