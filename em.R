#!/usr/bin/env Rscript

# Start this program via @./em.R@ (in the directory of the program)

# TODO modify sourceCpp to includes the full directory we are in
# TODO consider using rPackages.littler

library (flowCore)
library (flowViz)
library (ggplot2)
library (gtools)
library (mixtools)
library (mvtnorm)
library (optparse)
library (colortools)
library (Rcpp)

source("classes.R")
source("plotting.R")
source("writestats.R")
source("emwrapper.R")

parser <- OptionParser ()
parser <- add_option (parser, c ("-v", "--verbose"), action = "store_true", default=FALSE, help="be very verbose")
parser <- add_option (parser, c ("-f", "--file"), type="character", default = "", help="file to process")
parser <- add_option (parser, c ("-x", "--channelx"), type="character", default = "PMT.1", help="first channel")
parser <- add_option (parser, c ("-y", "--channely"), type="character", default = "PMT.9", help="second channel")
parser <- add_option (parser, c ("--xstart"), type="integer", default = 1500, help="")
parser <- add_option (parser, c ("--xend"), type="integer", default = 40000, help="")
parser <- add_option (parser, c ("--ystart"), type="integer", default = 5000, help="")
parser <- add_option (parser, c ("--yend"), type="integer", default = 38000, help="")
parser <- add_option (parser, c ("--fraction"), type="double", default = 0.02, help="")
parser <- add_option (parser, c ("--fracmult"), type="integer", default = 5, help="")
parser <- add_option (parser, c ("--prior"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--separation"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--inits"), type="integer", default = 10, help="")
parser <- add_option (parser, c ("--log"), action = "store_true", default=FALSE, help="")
parser <- add_option (parser, c ("--alpha"), type="double", default = 0.5, help="")
parser <- add_option (parser, c ("--imgformat"), type="character", default = "png", help="")
parser <- add_option (parser, c ("--startcluster"), type="integer", default = 2, help="")
parser <- add_option (parser, c ("--endcluster"), type="integer", default = 20, help="")

opts <- parse_args(parser)



# run the sampled algorithm
#
# Will, for a given number of clusters, start with the lowest fraction, find
# the EM-best cluster positions, then restart with higher fraction until all
# data have been used.

flowEMMiSingle<-function ( flowDataObject, fractions, numClusters, useLogScale, imageFormat, xMin, xMax, yMin, yMax)
{
  first<-TRUE
  em<-c()
  for (f in fractions)
  {
    pd <- mkFractionedFlowData (fdo=flowDataObject
                               ,fraction = f
                               ,xMin=xMin, xMax=xMax
                               ,yMin=yMin, yMax=yMax)
    plotInputData(pd, logScaled = useLogScale, imageFormat = imageFormat)
    if (first)
    {
      em <- iterateEM (deltaThreshold = 0.01, numClusters=numClusters, flowData = pd)
      first<-FALSE
    } else
    {
      em@logL <- -Inf
      em <- iterateInitedEM(em=em, deltaThreshold=0.01, numClusters=numClusters, flowData=pd)
    } # if(first)
  }
}

# run the complete flowEMMi algorithm

flowEMMi<-function( frame, ch1="FS.Log", ch2="FL.4.Log"
                         , xMin=0, xMax=4095,yMin=700,yMax=4095
                         ,useLogScale=TRUE,diff.ll=1
                         ,fraction=0.02
                         ,fracmult=5
                         ,minClusters=8,maxClusters=15,prior=FALSE
                         ,pi_prior,mu_prior,sigma_prior,separation=TRUE,max_inits=5,total=FALSE,alpha=.05,imageFormat="png",verbose=TRUE)
{
  #mat<-exprs(frame)
  stopifnot (fraction > 0.0)
  stopifnot (fracmult > 1.0)

  # the full flow data
  fdo <- mkFlowDataObject(frame=frame,xChannel=ch1, yChannel=ch2)

  # produce the vector of fractions, we want to run the algorithm with
  fractions <- c(fraction)
  repeat {
    l <- tail(fractions,n=1)
    if (l>=1) break
    fractions <- c(fractions, min(1.0, l * fracmult))
  }
  flowEMMiSingle( flowDataObject=fdo, fractions=fractions
                 , numClusters=minClusters, useLogScale=useLogScale, imageFormat=imageFormat
                 , xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax)
  error()

  # for each fraction, run the flowEMMi algorithm
  for (f in fractions)
  {
    #pd <- mkFractionedFlowData (fdo=fdo
    #                           ,fraction = f
    #                           ,xMin=xMin, xMax=xMax
    #                           ,yMin=yMin, yMax=yMax)

    #plotInputData(pd, logScaled = useLogScale, imageFormat = imageFormat)

    BIC<-rep(0,maxClusters)
    palette <- wheel ("steelblue", num = maxClusters)
    act_T<-list()
    act_P_mat<-list()
    act_pi<-list()
    pis<-list()
    act_mu<-list()
    mus<-list()
    act_sigma<-list()
    sigmas<-list()
    act_loglik<-list()
    act_iterations<-list()
    probs<-list()
    ll<-list()
    newList<-list()

    # TODO parallelization (but consider what exactly to parallelize)
    #
    # numCores <- detectCores() # -1
    # cluster  <- makeCluster(numCores)
    # parLapply (cluster, minClusters:maxClusters, functionToCall)

    for(c in minClusters:maxClusters)
    {
      #ll[c][1]<-0
      #counter<-2
      for (iteration in 1:max_inits)
      {
        # initialize the EM with lowest count of elements
        em <- iterateEM (deltaThreshold = 0.01, numClusters = c, flowData = pd)
        # increase element count
        # for each initialization, run the algorithm
        if (prior) {
          # this variant assumes that we collected start points from somewhere else
          # TODO re-use iterateEM ...
        } else {
          # this variant randomly selects start points for the EM algorithm
        }
      } # initiations
    } # for minClusters ... maxClusters
    plotBIC (newList$BIC)
    statBIC (newList$BIC)
    for(c in minClusters:maxClusters)
    {
      statCluster(newList$mu[[c]], newList$sigma[[c]])
    }
  } # for f in fractions
} # flowEMMi


# load sample
fcsData <- read.FCS(opts$file,alter.names = TRUE,transformation = FALSE)
# run actual flowEMMi algorithm
results <- flowEMMi( frame = fcsData
                   , ch1=opts$channelx, ch2=opts$channely
                   , xMin = opts$xstart, xMax = opts$xend, yMin=opts$ystart, yMax=opts$yend
                   , fraction = opts$fraction
                   , fracmult = opts$fracmult
                   , prior = opts$prior
                   , separation = opts$separation
                   , max_inits = opts$inits
                   , useLogScale = opts$log
                   , alpha = opts$alpha, imageFormat = opts$imgformat
                   , minClusters = opts$startcluster, maxClusters = opts$endcluster
                   )

